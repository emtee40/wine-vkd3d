/*
 * Copyright 2024 Stefan Dösinger for CodeWeavers
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 */

#include "vkd3d_private.h"

#ifdef HAVE_SHARE_H
#include <share.h>
#endif

/* Data structures used in the serialized files. Changing these will break compatibility with
 * existing cache files, so bump the cache version if doing so.
 *
 * We don't intend these files to be read by third party code, consider them a vkd3d implementation
 * detail.
 *
 * The magic deliberately has no endianess correction. If transfering cache files from big to
 * little endian machines is desired, the data written into the cache (d3d12 pipelines, etc) will
 * need to be adjusted too. This is not something we want to do unless we have an actual use for it.
 * (As of the time of this writing native d3d12 even throws away the cache contents when switching
 * between 32 bit and 64 bit programs that access it.) */

static const uint64_t VKD3D_SHADER_CACHE_MAGIC = 0x564B443344534843ull; /* VKD3DSHC */
static const uint64_t VKD3D_SHADER_CACHE_VERSION = 1ull;

struct vkd3d_cache_header_v1
{
    uint64_t magic;
    uint64_t struct_size;
    uint64_t vkd3d_version;
    uint64_t app_version;
};

/* What is the (future) idea behind the 'access' field? Have a global counter in struct
 * vkd3d_shader_cache that is incremented on every read and write. Set access to match that counter
 * when a cache entry is read or modified. If we run out of space, create a separate tree sorted by
 * access time and delete oldest objects until the cache fits the constraints again.
 *
 * On load from disk, the cache's access time can be set to the highest time found in the stored
 * items. There should be no need to write the cache-global counter separately. We want to remember
 * the counter found at load time though to use it later to find out which elements were modified
 * and selectively write them to out.
 *
 * We could keep separate read and write counters, e.g. by splitting the uint64_t into two
 * uint32_t's, but I don't think this is worth it. For one, changing the read counter modifies the
 * cache item and this change needs to be written back to be useful. It could be used to avoid
 * re-writing the payload after reads though. */

struct vkd3d_cache_entry_header_v1
{
    uint64_t hash;
    uint64_t access; /* For future LRU eviction. */
    uint64_t disk_size; /* Size of the payload in the file. May be compressed. */
    uint64_t key_size; /* Size of the app provided key. */
    uint64_t value_size; /* Size of the value. key_size + value_size = uncompressed entry size. */
};

/* end of on disk data structures */

struct vkd3d_shader_cache
{
    unsigned int refcount;
    struct vkd3d_mutex lock;

    struct vkd3d_shader_cache_info info;
    struct rb_tree tree;

    FILE *file;
    bool delete_on_destroy;

    char filename[];
};

struct shader_cache_entry
{
    struct vkd3d_cache_entry_header_v1 h;
    struct rb_entry entry;
    uint8_t *payload;
};

struct shader_cache_key
{
    uint64_t hash;
    const void *key;
    uint64_t key_size;
};

static int vkd3d_shader_cache_compare_key(const void *key, const struct rb_entry *entry)
{
    const struct shader_cache_entry *e = RB_ENTRY_VALUE(entry, struct shader_cache_entry, entry);
    const struct shader_cache_key *k = key;
    int ret;

    if ((ret = vkd3d_u64_compare(k->hash, e->h.hash)))
        return ret;
    if ((ret = vkd3d_u64_compare(k->key_size, e->h.key_size)))
        return ret;

    /* Until now we have not seen an actual hash collision. If the key didn't match it was always
     * due to a bug in the serialization code or memory corruption. If you see this FIXME please
     * investigate. */
    if ((ret = memcmp(k->key, e->payload, k->key_size)))
        FIXME("Actual case of a hash collision found.\n");
    return ret;
}

static void vkd3d_shader_cache_add_entry(struct vkd3d_shader_cache *cache,
        struct shader_cache_entry *e)
{
    rb_put(&cache->tree, &e->h.hash, &e->entry);
}

static void vkd3d_shader_cache_remove_entry(struct vkd3d_shader_cache *cache,
        struct shader_cache_entry *e)
{
    rb_remove(&cache->tree, &e->entry);
}

uint64_t vkd3d_shader_cache_hash_key(const void *key, size_t size)
{
    static const uint64_t fnv_prime = 0x00000100000001b3;
    uint64_t hash = 0xcbf29ce484222325;
    const uint8_t *k = key;
    size_t i;

    for (i = 0; i < size; ++i)
        hash = (hash ^ k[i]) * fnv_prime;

    return hash;
}

static bool vkd3d_shader_cache_read_entry(struct vkd3d_shader_cache *cache, struct shader_cache_entry *e)
{
    uint64_t hash;
    size_t len;

    TRACE("reading object key len %#"PRIx64", data %#"PRIx64".\n", e->h.key_size, e->h.value_size);

    e->payload = vkd3d_malloc(e->h.key_size + e->h.value_size);
    if (!e->payload)
        return false;

    if (e->h.disk_size != e->h.key_size + e->h.value_size)
        ERR("How do I get a compressed object before implementing compression?\n");

    len = fread(e->payload, e->h.key_size + e->h.value_size, 1, cache->file);
    if (len != 1)
    {
        FIXME("Failed to read cached object data len %#"PRIx64".\n",
                e->h.key_size + e->h.value_size);
        vkd3d_free(e->payload);
        return false;
    }

    hash = vkd3d_shader_cache_hash_key(e->payload, e->h.key_size);
    if (hash != e->h.hash)
        ERR("Key hash %#"PRIx64" does not match header value %#"PRIx64".\n", hash, e->h.hash);

    return true;
}

static FILE *fopen_wrapper(const char *filename, const char *mode)
{
#if defined(HAVE__FSOPEN) && defined(HAVE_SHARE_H)
    return _fsopen(filename, mode, _SH_DENYRW);
#else
    return fopen(filename, mode);
#endif
}

static bool vkd3d_shader_cache_read(struct vkd3d_shader_cache *cache)
{
    struct shader_cache_entry *e = NULL;
    struct vkd3d_cache_header_v1 hdr;
    off64_t cache_file_size = 0, p;
    char *filename;
    size_t len;

    filename = vkd3d_malloc(strlen(cache->filename) + 5);
    if (!filename)
        return false;

    sprintf(filename, "%s.bin", cache->filename);
    cache->file = fopen_wrapper(filename, "r+b");
    if (!cache->file)
    {
        cache->file = fopen_wrapper(filename, "w+b");
        if (!cache->file)
        {
            WARN("Shader cache file %s not found and could not be created.\n", filename);
            return false;
        }
    }

    /* FIXME: I think fseeko64 / ftello64 are exist only in MinGW on Windows. */
    fseeko64(cache->file, 0, SEEK_END);
    cache_file_size = ftello64(cache->file);
    fseeko64(cache->file, 0, SEEK_SET);

    TRACE("Reading cache %s.\n", filename);
    vkd3d_free(filename);

    len = fread(&hdr, sizeof(hdr), 1, cache->file);
    if (len != 1)
    {
        WARN("Failed to read cache header.\n");
        goto done;
    }
    if (hdr.magic != VKD3D_SHADER_CACHE_MAGIC)
    {
        WARN("Invalid cache magic.\n");
        goto done;
    }
    if (hdr.struct_size < sizeof(hdr))
    {
        WARN("Invalid cache header size.\n");
        goto done;
    }
    if (hdr.vkd3d_version != VKD3D_SHADER_CACHE_VERSION)
    {
        WARN("vkd3d shader version mismatch: Got %"PRIu64", want %"PRIu64".\n",
                hdr.vkd3d_version, VKD3D_SHADER_CACHE_VERSION);
        goto done;
    }
    if (hdr.app_version != cache->info.version)
    {
        WARN("Application version mismatch: Cache has %"PRIu64", app wants %"PRIu64".\n",
                hdr.app_version, cache->info.version);
        goto done;
    }

    while (!feof(cache->file))
    {
        e = vkd3d_calloc(1, sizeof(*e));
        if (!e)
            break;

        len = fread(&e->h, sizeof(e->h), 1, cache->file);
        if (len != 1)
        {
            if (!feof(cache->file))
                ERR("Failed to read object header.\n");
            break;
        }

        if (e->h.key_size + e->h.value_size > ~(size_t)0)
        {
            FIXME("Shader object is larger than the address space, aborting load.\n");
            break;
        }

        p = ftello64(cache->file);
        if (e->h.disk_size > cache_file_size || p > (cache_file_size - e->h.disk_size))
        {
            FIXME("Cache corrupt: Offset %#"PRIx64", size %#"PRIx64", file size %#"PRIx64".\n",
                    p, e->h.disk_size, cache_file_size);
            break;
        }

        if (!vkd3d_shader_cache_read_entry(cache, e))
            break;

        vkd3d_shader_cache_add_entry(cache, e);

        TRACE("Loaded an entry.\n");
        e = NULL;
    }

done:
    vkd3d_free(e);
    return true;
}

int vkd3d_shader_open_cache(const struct vkd3d_shader_cache_info *info,
        struct vkd3d_shader_cache **cache)
{
    struct vkd3d_shader_cache *object;
    size_t size;

    TRACE("%p.\n", cache);

    size = info->filename ? strlen(info->filename) + 1 : 1;
    object = vkd3d_malloc(offsetof(struct vkd3d_shader_cache, filename[size]));
    if (!object)
        return VKD3D_ERROR_OUT_OF_MEMORY;

    object->refcount = 1;
    vkd3d_mutex_init(&object->lock);
    object->info = *info;
    rb_init(&object->tree, vkd3d_shader_cache_compare_key);
    object->file = NULL;
    object->delete_on_destroy = false;
    object->filename[0] = '\0';

    if (info->filename)
    {
        memcpy(object->filename, info->filename, size);
        object->info.filename = object->filename;
        if (!vkd3d_shader_cache_read(object))
        {
            vkd3d_free(object);
            return VKD3D_ERROR;
        }
    }

    *cache = object;

    return VKD3D_OK;
}

unsigned int vkd3d_shader_cache_incref(struct vkd3d_shader_cache *cache)
{
    unsigned int refcount = vkd3d_atomic_increment_u32(&cache->refcount);
    TRACE("cache %p refcount %u.\n", cache, refcount);
    return refcount;
}

static void vkd3d_shader_cache_destroy_entry(struct rb_entry *entry, void *context)
{
    struct shader_cache_entry *e = RB_ENTRY_VALUE(entry, struct shader_cache_entry, entry);
    vkd3d_free(e->payload);
    vkd3d_free(e);
}

static void vkd3d_shader_cache_write_entry(struct rb_entry *entry, void *context)
{
    struct shader_cache_entry *e = RB_ENTRY_VALUE(entry, struct shader_cache_entry, entry);
    struct vkd3d_shader_cache *cache = context;

    /* TODO: Compress the data. */
    e->h.disk_size = e->h.key_size + e->h.value_size;
    fwrite(&e->h, sizeof(e->h), 1, cache->file);
    fwrite(e->payload, e->h.disk_size, 1, cache->file);
}

static void vkd3d_shader_cache_write(struct vkd3d_shader_cache *cache)
{
    struct vkd3d_cache_header_v1 hdr;
    char *filename, *dstname;
    int ret;

    filename = vkd3d_malloc(strlen(cache->filename) + 9);
    dstname = vkd3d_malloc(strlen(cache->filename) + 5);
    if (!filename || !dstname)
        goto done;

    if (cache->delete_on_destroy)
    {
        fclose(cache->file);

        sprintf(filename, "%s.bin", cache->filename);
        remove(filename);
        goto done;
    }

    /* For now unconditionally repack. */
    fclose(cache->file);
    sprintf(filename, "%s-new.bin", cache->filename);
    cache->file = fopen_wrapper(filename, "wb");
    if (!cache->file)
    {
        WARN("Failed to open %s\n", filename);
        goto done;
    }

    hdr.magic = VKD3D_SHADER_CACHE_MAGIC;
    hdr.struct_size = sizeof(hdr);
    hdr.vkd3d_version = VKD3D_SHADER_CACHE_VERSION;
    hdr.app_version = cache->info.version;

    fwrite(&hdr, sizeof(hdr), 1, cache->file);

    rb_for_each_entry(&cache->tree, vkd3d_shader_cache_write_entry, cache);

    fclose(cache->file);

    sprintf(dstname, "%s.bin", cache->filename);
    remove(dstname); /* msvcrt needs this. */
    ret = rename(filename, dstname);
    if (ret)
        ERR("Cache file rename failed.\n");

done:
    vkd3d_free(filename);
    vkd3d_free(dstname);
}

unsigned int vkd3d_shader_cache_decref(struct vkd3d_shader_cache *cache)
{
    unsigned int refcount = vkd3d_atomic_decrement_u32(&cache->refcount);
    TRACE("cache %p refcount %u.\n", cache, refcount);

    if (refcount)
        return refcount;

    if (cache->file)
        vkd3d_shader_cache_write(cache);

    rb_destroy(&cache->tree, vkd3d_shader_cache_destroy_entry, NULL);
    vkd3d_mutex_destroy(&cache->lock);

    vkd3d_free(cache);
    return 0;
}

static void vkd3d_shader_cache_lock(struct vkd3d_shader_cache *cache)
{
    if (cache->info.flags & VKD3D_SHADER_CACHE_FLAGS_NO_SERIALIZE)
        return;

    vkd3d_mutex_lock(&cache->lock);
}

static void vkd3d_shader_cache_unlock(struct vkd3d_shader_cache *cache)
{
    if (cache->info.flags & VKD3D_SHADER_CACHE_FLAGS_NO_SERIALIZE)
        return;

    vkd3d_mutex_unlock(&cache->lock);
}

int vkd3d_shader_cache_put(struct vkd3d_shader_cache *cache, const void *key, size_t key_size,
        const void *value, size_t value_size, enum vkd3d_shader_cache_put_flags flags)
{
    struct shader_cache_entry *e;
    struct shader_cache_key k;
    struct rb_entry *entry;
    enum vkd3d_result ret;

    TRACE("%p, %p, %#zx, %p, %#zx.\n", cache, key, key_size, value, value_size);

    k.hash = vkd3d_shader_cache_hash_key(key, key_size);
    k.key = key;
    k.key_size = key_size;

    vkd3d_shader_cache_lock(cache);

    entry = rb_get(&cache->tree, &k);
    e = entry ? RB_ENTRY_VALUE(entry, struct shader_cache_entry, entry) : NULL;

    if (e)
    {
        if (flags & VKD3D_PUT_REPLACE)
        {
            vkd3d_shader_cache_remove_entry(cache, e);
            vkd3d_free(e);
        }
        else
        {
            WARN("Key already exists, returning VKD3D_ERROR_KEY_ALREADY_EXISTS.\n");
            ret = VKD3D_ERROR_KEY_ALREADY_EXISTS;
            goto done;
        }
    }

    e = vkd3d_malloc(sizeof(*e));
    if (!e)
    {
        ret = VKD3D_ERROR_OUT_OF_MEMORY;
        goto done;
    }
    e->payload = vkd3d_malloc(key_size + value_size);
    if (!e->payload)
    {
        vkd3d_free(e);
        ret = VKD3D_ERROR_OUT_OF_MEMORY;
        goto done;
    }

    e->h.key_size = key_size;
    e->h.value_size = value_size;
    e->h.hash = k.hash;
    e->h.disk_size = 0;
    e->h.access = 0;
    memcpy(e->payload, key, key_size);
    memcpy(e->payload + key_size, value, value_size);

    vkd3d_shader_cache_add_entry(cache, e);
    TRACE("Cache entry %#"PRIx64" stored.\n", k.hash);
    ret = VKD3D_OK;

done:
    vkd3d_shader_cache_unlock(cache);
    return ret;
}

int vkd3d_shader_cache_get(struct vkd3d_shader_cache *cache,
        const void *key, size_t key_size, void *value, size_t *value_size)
{
    struct shader_cache_entry *e;
    struct shader_cache_key k;
    struct rb_entry *entry;
    enum vkd3d_result ret;
    size_t size_in;

    TRACE("%p, %p, %#zx, %p, %p.\n", cache, key, key_size, value, value_size);

    size_in = *value_size;

    k.hash = vkd3d_shader_cache_hash_key(key, key_size);
    k.key = key;
    k.key_size = key_size;

    vkd3d_shader_cache_lock(cache);

    entry = rb_get(&cache->tree, &k);
    if (!entry)
    {
        WARN("Entry not found.\n");
        ret = VKD3D_ERROR_NOT_FOUND;
        goto done;
    }

    e = RB_ENTRY_VALUE(entry, struct shader_cache_entry, entry);

    *value_size = e->h.value_size;
    if (!value)
    {
        TRACE("Found item %#"PRIx64", returning needed size %#"PRIx64".\n",
                e->h.hash, e->h.value_size);
        ret = VKD3D_OK;
        goto done;
    }

    if (size_in < e->h.value_size)
    {
        WARN("Output buffer is too small for item %#"PRIx64", got %#zx want %#"PRIx64".\n",
                e->h.hash, size_in, e->h.value_size);
        ret = VKD3D_ERROR_MORE_DATA;
        goto done;
    }

    memcpy(value, e->payload + e->h.key_size, e->h.value_size);
    ret = VKD3D_OK;
    TRACE("Returning cached item %#"PRIx64".\n", e->h.hash);

done:
    vkd3d_shader_cache_unlock(cache);
    return ret;
}

void vkd3d_shader_cache_delete_on_destroy(struct vkd3d_shader_cache *cache)
{
    TRACE("%p\n", cache);
    vkd3d_shader_cache_lock(cache);
    cache->delete_on_destroy = true;
    vkd3d_shader_cache_unlock(cache);
}

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* If a process creates multiple devices they share the disk cache.
 *
 * The vkd3d_instance structure seems like a good place to store this, except that Wine's d3d12
 * creates one vkd3d_instance for each d3d12 device.
 *
 * I suspect Microsoft invented the weird cache sharing for this reason. It allows multiple d3d12
 * devices to share one cache without having an ugly-looking global variable. */
struct vkd3d_cache_struct persistent_cache =
{
    VKD3D_MUTEX_INITIALIZER, NULL,
    {0, 0, NULL},
    {0, 0, NULL},
};

HRESULT vkd3d_persistent_cache_open(const struct vkd3d_instance *instance)
{
    struct vkd3d_shader_cache_info cache_info = {0};
    uint64_t hash, *hashes = NULL;
    char *cache_name, *cwd;
    unsigned int i, count;
    enum vkd3d_result ret;
    size_t alloc_size, size;
    HRESULT hr = S_OK;

    /* FIXME: Does this use of getcwd work on Unix too? */
    cwd = getcwd(NULL, 0);
    cache_name = vkd3d_malloc(strlen(cwd) + strlen(instance->application_name) + 8);
    sprintf(cache_name, "%s/%s.cache", cwd, instance->application_name);
    free(cwd); /* Use libc's free() because it is malloc'ed by getcwd. */

    vkd3d_mutex_lock(&persistent_cache.mutex);
    cache_info.version = VKD3D_SHADER_CACHE_OBJ_VERSION;
    cache_info.filename = cache_name;
    cache_info.flags = VKD3D_SHADER_CACHE_FLAGS_NO_SERIALIZE;
    if (persistent_cache.cache)
    {
        vkd3d_shader_cache_incref(persistent_cache.cache);
        goto out;
    }
    else if (vkd3d_shader_open_cache(&cache_info, &persistent_cache.cache))
    {
        FIXME("Failed to open shader cache %s.\n", debugstr_a(cache_name));
        cache_info.filename = NULL;
        if (vkd3d_shader_open_cache(&cache_info, &persistent_cache.cache))
        {
            FIXME("Failed to open a dummy memory cache.\n");
            hr = E_FAIL;
        }
        /* We won't find anything in the dummy cache. */
        goto out;
    }
    else if ((ret = vkd3d_shader_cache_get(persistent_cache.cache,
                vkd3d_root_signature_index, sizeof(vkd3d_root_signature_index), NULL, &alloc_size)))
    {
        /* OK if the cache is empty. Downgrade to a WARN later. */
        FIXME("Root signature index not found.\n");
        goto out;
    }
    /* We want the application to be able to create new pipelines while the cache load is running.
     * We want to record those new and not yet cached pipelines in the cache and merge the list of
     * pipelines. I see four options:
     *
     * 1: Make the root_signature_create call() add them. That won't work easily because the
     * pipeline hash already exists in the disk cache, so root_signature_create() will get
     * VKD3D_ERROR_KEY_ALREADY_EXISTS.
     *
     * 2.a: Assign the array we have here into persistent_cache.root_signatures.a. As long as this
     * function has exclusive control over the cache this should work, but gives us no way of
     * removing evicted hashes.
     *
     * 2.b: Iterate over the hashes here and add the ones we find.
     *
     * 3.a: Do 2.b in device_load_cache(), removing one loop over the cache. A problem with this is
     * that device_load_cache is called for each created device, but we want to share the cache
     * between multiple devices in the same process. So we have to make sure that hashes are copied
     * along only the first time, otherwise we'd duplicate everything.
     *
     * 3.b: Do 2.a here and have device_load_cache() delete every not found entry. Deleting would
     * need a different data structure than an array though.
     *
     * Right now it is #2.b
     */
    hashes = vkd3d_malloc(alloc_size);
    if (!hashes)
    {
        hr = E_OUTOFMEMORY;
        goto out;
    }
    size = alloc_size;
    ret = vkd3d_shader_cache_get(persistent_cache.cache, vkd3d_root_signature_index,
            sizeof(vkd3d_root_signature_index), hashes, &size);
    if (ret)
        ERR("huh fail\n");

    count = size / sizeof(*hashes);
    for (i = 0; i < count; ++i)
    {
        hash = hashes[i];

        ret = vkd3d_shader_cache_get(persistent_cache.cache, &hash, sizeof(hash), NULL, &size);
        if (ret == VKD3D_ERROR_NOT_FOUND)
        {
            /* We don't have eviction yet, so this likely indicates a bug. */
            FIXME("Root signature with hash %#"PRIx64" was evicted.\n", hash);
            continue;
        }
        else
        {
            TRACE("Re-add root signature %#"PRIx64".\n", hash);
            vkd3d_dynamic_array_put(&persistent_cache.root_signatures, hash);
        }
    }
    ERR("on load %zu root signatures in array\n", persistent_cache.root_signatures.count);

    size = alloc_size;
    ret = vkd3d_shader_cache_get(persistent_cache.cache,
            vkd3d_graphics_index, sizeof(vkd3d_graphics_index), hashes, &size);
    if (ret == VKD3D_ERROR_MORE_DATA)
    {
        hashes = vkd3d_realloc(hashes, size);
        if (!hashes)
        {
            hr = E_OUTOFMEMORY;
            ERR("Out of memory\n");
            goto out;
        }
        alloc_size = size;
        if ((ret = vkd3d_shader_cache_get(persistent_cache.cache,
                vkd3d_graphics_index, sizeof(vkd3d_graphics_index), hashes, &size)))
            ERR("huh?\n");
    }
    else if (ret && ret != VKD3D_ERROR_MORE_DATA)
    {
        FIXME("Graphics pipeline collection not found.\n");
        goto out;
    }

    count = size / sizeof(*hashes);
    for (i = 0; i < count; ++i)
    {
        hash = hashes[i];

        ret = vkd3d_shader_cache_get(persistent_cache.cache, &hash, sizeof(hash), NULL, &size);
        if (ret == VKD3D_ERROR_NOT_FOUND)
        {
            /* We don't have eviction yet, so this likely indicates a bug. */
            FIXME("Graphics pipeline with hash %#"PRIx64" was evicted.\n", hash);
            continue;
        }
        else
        {
            TRACE("Re-add graphics pipeline %#"PRIx64".\n", hash);
            vkd3d_dynamic_array_put(&persistent_cache.graphics_pipelines, hash);
        }
    }
    ERR("on load %zu graphics pipelines in array\n", persistent_cache.graphics_pipelines.count);

out:
    vkd3d_free(hashes);
    vkd3d_free(cache_name);
    vkd3d_mutex_unlock(&persistent_cache.mutex);
    return hr;
}

void vkd3d_persistent_cache_close(void)
{
    unsigned int cache_ref;
    enum vkd3d_result ret;

    vkd3d_mutex_lock(&persistent_cache.mutex);

    /* FIXME: We don't need to close the cache here, but at this point closing
     * it is the only way to initiate a write to disk. */
    ERR("on close %zu root sigs in array\n", persistent_cache.root_signatures.count);
    ret = vkd3d_shader_cache_put(persistent_cache.cache, vkd3d_root_signature_index,
            sizeof(vkd3d_root_signature_index), persistent_cache.root_signatures.a,
            persistent_cache.root_signatures.count * sizeof(*persistent_cache.root_signatures.a),
            VKD3D_PUT_REPLACE);
    if (ret)
        ERR("Failed to store root signature index object.\n");

    ERR("on close %zu gfx pipes in array\n", persistent_cache.graphics_pipelines.count);
    ret = vkd3d_shader_cache_put(persistent_cache.cache, vkd3d_graphics_index,
            sizeof(vkd3d_graphics_index), persistent_cache.graphics_pipelines.a,
            persistent_cache.graphics_pipelines.count * sizeof(*persistent_cache.graphics_pipelines.a),
            VKD3D_PUT_REPLACE);
    if (ret)
        ERR("Failed to store root signature index object.\n");

    cache_ref = vkd3d_shader_cache_decref(persistent_cache.cache);
    if (!cache_ref)
    {
        persistent_cache.root_signatures.count = 0;
        persistent_cache.graphics_pipelines.count = 0;
        persistent_cache.cache = NULL;
    }
    vkd3d_mutex_unlock(&persistent_cache.mutex);
}

void vkd3d_persistent_cache_add_root_signature(const struct d3d12_root_signature *root_signature)
{
    enum vkd3d_result ret;

    vkd3d_mutex_lock(&persistent_cache.mutex);

    ret = vkd3d_shader_cache_put(persistent_cache.cache, &root_signature->hash,
            sizeof(root_signature->hash), root_signature->bytecode,
            root_signature->bytecode_length, 0);
    if (!ret)
    {
        vkd3d_dynamic_array_put(&persistent_cache.root_signatures, root_signature->hash);
        TRACE("Added root signature %#"PRIx64"\n", root_signature->hash);
    }
    else if (ret == VKD3D_ERROR_KEY_ALREADY_EXISTS)
    {
        TRACE("Root signature %#"PRIx64" already stored in cache\n", root_signature->hash);
    }
    else
    {
        ERR("Unexpected error adding root signature to cache.\n");
    }

    vkd3d_mutex_unlock(&persistent_cache.mutex);
}

void vkd3d_persistent_cache_add_graphics_pipeline(const struct vkd3d_graphics_pipeline_entry *entry)
{
    enum vkd3d_result ret;
    uint64_t hash = vkd3d_shader_cache_hash_key(entry, sizeof(*entry));

    vkd3d_mutex_lock(&persistent_cache.mutex);

    ret = vkd3d_shader_cache_put(persistent_cache.cache, &hash,
            sizeof(hash), entry, sizeof(*entry), 0);
    if (!ret)
    {
        vkd3d_dynamic_array_put(&persistent_cache.graphics_pipelines, hash);
        TRACE("Added graphics pipeline %#"PRIx64"\n", hash);
    }
    else if (ret == VKD3D_ERROR_KEY_ALREADY_EXISTS)
    {
        TRACE("Graphics pipeline %#"PRIx64" already stored in cache\n", hash);
    }
    else
    {
        ERR("Unexpected error adding root signature to cache.\n");
    }

    vkd3d_mutex_unlock(&persistent_cache.mutex);
}

