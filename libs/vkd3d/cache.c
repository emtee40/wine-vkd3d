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

/* Data structures used in the serialized files. Changing these will break compatibility with
 * existing cache files, so bump the cache version if doing so.
 *
 * We don't intend these files to be read by third party code, so consider them a vkd3d
 * implementation detail. */

/* TODO: Endinaness of all these uints. */

/* VKD3DSHC */
#define VKD3D_SHADER_CACHE_MAGIC 0x564B443344534843ull
#define VKD3D_SHADER_CACHE_VERSION ((uint64_t)1)

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
 * the counter found at load time though to use it later to find out which elements were modified#
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
    uint64_t offset; /* Where key + value are located in the .val file. */
    uint64_t disk_size; /* Size of the entry in the .val file. May be compressed. */
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

    FILE *indices, *values;

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

static bool vkd3d_shader_cache_read_entry(struct vkd3d_shader_cache *cache, struct shader_cache_entry *e)
{
    size_t len;

    TRACE("reading object key len %#"PRIx64", data %#"PRIx64".\n", e->h.key_size, e->h.value_size);

    if (e->h.key_size + e->h.value_size > ~(size_t)0)
    {
        /* Returning false here aborts loading more objects. Technically we might want
         * to continue and load other objects or stop creating the cache. Right now we
         * also re-write the cache on close, so we throw away data. */
        FIXME("Shader object is larger than the address space, aborting load.\n");
        return false;
    }

    /* TODO: Check if the read size makes sense - is it smaller than the requested
     * max size, is it smaller than the file on the disk etc. */
    e->payload = vkd3d_malloc(e->h.key_size + e->h.value_size);
    if (!e->payload)
    {
        WARN("Out of memory.\n");
        return false;
    }

    if (e->h.disk_size != e->h.key_size + e->h.value_size)
        ERR("How do I get a compressed object before implementing compression?\n");

    fseek(cache->values, e->h.offset, SEEK_SET);
    len = fread(e->payload, e->h.key_size + e->h.value_size, 1, cache->values);
    if (len != 1)
    {
        /* I suppose this could be handled better. */
        ERR("Failed to read cached object data len %#"PRIx64" offset %#"PRIx64".\n",
                e->h.key_size + e->h.value_size, e->h.offset);
        vkd3d_free(e->payload);
        return false;
    }

    return true;
}

static void vkd3d_shader_cache_read(struct vkd3d_shader_cache *cache)
{
    const bool ro = cache->info.flags & VKD3D_SHADER_CACHE_FLAGS_READ_ONLY;
    struct shader_cache_entry *e = NULL;
    struct vkd3d_cache_header_v1 hdr;
    char *filename;
    FILE *indices;
    size_t len;

    filename = vkd3d_malloc(strlen(cache->filename) + 5);

    sprintf(filename, "%s.val", cache->filename);
    cache->values = fopen(filename, ro ? "rb" : "r+b");
    if (!cache->values)
    {
        if (ro)
        {
            WARN("Read only cache file %s not found.\n", filename);
            return;
        }

        cache->values = fopen(filename, "w+b");
        if (!cache->values)
        {
            WARN("Value file %s not found and could not be created.\n", filename);
             /* Convert to mem only. */
            cache->filename[0] = '\0';
            vkd3d_free(filename);
            return;
        }
    }

    sprintf(filename, "%s.idx", cache->filename);
    indices = fopen(filename, "rb");
    if (!indices)
    {
        /* This happens when the cache files did not exist. Keep the opened
         * values file, we'll use it later. */
        WARN("Index file %s not found.\n", filename);
        vkd3d_free(filename);
        return;
    }

    vkd3d_free(filename);

    TRACE("Reading cache %s.{idx, val}.\n", cache->filename);

    len = fread(&hdr, sizeof(hdr), 1, indices);
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

    while (!feof(indices))
    {
        e = vkd3d_calloc(1, sizeof(*e));
        if (!e)
        {
            WARN("Alloc fail.\n");
            break;
        }

        len = fread(&e->h, sizeof(e->h), 1, indices);
        if (len != 1)
        {
            if (!feof(indices))
                ERR("Failed to read object header.\n");
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
    fclose(indices);
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
    object->indices = object->values = NULL;
    object->filename[0] = '\0';

    if (info->filename)
    {
        memcpy(object->filename, info->filename, size);
        object->info.filename = object->filename;
        vkd3d_shader_cache_read(object);
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

struct write_context
{
    struct vkd3d_shader_cache *cache;
    FILE *indices;
};

static void vkd3d_shader_cache_write_entry(struct rb_entry *entry, void *context)
{
    struct shader_cache_entry *e = RB_ENTRY_VALUE(entry, struct shader_cache_entry, entry);
    struct write_context *ctx = context;
    struct vkd3d_shader_cache *cache = ctx->cache;

    /* TODO: Compress the data. */
    e->h.disk_size = e->h.key_size + e->h.value_size;
    e->h.offset = ftell(cache->values);

    fwrite(&e->h, sizeof(e->h), 1, ctx->indices);
    fwrite(e->payload, e->h.disk_size, 1, cache->values);
}

static void vkd3d_shader_cache_write(struct vkd3d_shader_cache *cache)
{
    struct vkd3d_cache_header_v1 hdr;
    struct write_context ctx;
    char *filename, *dstname;
    int ret;

    if (cache->info.flags & VKD3D_SHADER_CACHE_FLAGS_READ_ONLY)
    {
        fclose (cache->values);
        return;
    }

    fseek(cache->values, 0, SEEK_END);

    filename = vkd3d_malloc(strlen(cache->filename) + 9);
    dstname = vkd3d_malloc(strlen(cache->filename) + 5);
    if (!filename || !dstname)
        goto out;

    /* For now unconditionally repack. */
    fclose(cache->values);
    sprintf(filename, "%s-new.val", cache->filename);
    cache->values = fopen(filename, "w+b");
    if (!cache->values)
    {
        WARN("Failed to open %s\n", filename);
        goto out;
    }

    sprintf(filename, "%s-new.idx", cache->filename);
    ctx.indices = fopen(filename, "wb");
    if (!ctx.indices)
    {
        WARN("Failed to open %s\n", filename);
        fclose(cache->values);
        goto out;
    }

    ctx.cache = cache;
    hdr.magic = VKD3D_SHADER_CACHE_MAGIC;
    hdr.struct_size = sizeof(hdr);
    hdr.vkd3d_version = VKD3D_SHADER_CACHE_VERSION;
    hdr.app_version = cache->info.version;

    fwrite(&hdr, sizeof(hdr), 1, ctx.indices);

    rb_for_each_entry(&cache->tree, vkd3d_shader_cache_write_entry, &ctx);

    fseek(cache->values, 0, SEEK_END);
    fclose(cache->values);
    fclose(ctx.indices);

    sprintf(dstname, "%s.idx", cache->filename);
    remove(dstname); /* msvcrt needs this. */
    ret = rename(filename, dstname);
    if (ret)
        ERR("Index file rename failed.\n");

    sprintf(dstname, "%s.val", cache->filename);
    sprintf(filename, "%s-new.val", cache->filename);
    remove(dstname);
    rename(filename, dstname);
    if (ret)
        ERR("Value file rename failed.\n");

out:
    vkd3d_free(filename);
    vkd3d_free(dstname);
}

unsigned int vkd3d_shader_cache_decref(struct vkd3d_shader_cache *cache)
{
    unsigned int refcount = vkd3d_atomic_decrement_u32(&cache->refcount);
    TRACE("cache %p refcount %u.\n", cache, refcount);

    if (refcount)
        return refcount;

    if (cache->values)
        vkd3d_shader_cache_write(cache);

    rb_destroy(&cache->tree, vkd3d_shader_cache_destroy_entry, NULL);
    vkd3d_mutex_destroy(&cache->lock);

    vkd3d_free(cache);
    return 0;
}

static uint64_t vkd3d_shader_cache_hash_key(const void *key, size_t size)
{
    static const uint64_t fnv_prime = 0x00000100000001b3;
    uint64_t hash = 0xcbf29ce484222325;
    const uint8_t *k = key;
    size_t i;

    for (i = 0; i < size; ++i)
        hash = (hash ^ k[i]) * fnv_prime;

    return hash;
}

static void vkd3d_shader_cache_lock(struct vkd3d_shader_cache *cache)
{
    if (cache->info.flags & (VKD3D_SHADER_CACHE_FLAGS_NO_SERIALIZE | VKD3D_SHADER_CACHE_FLAGS_READ_ONLY))
        return;

    vkd3d_mutex_lock(&cache->lock);
}

static void vkd3d_shader_cache_unlock(struct vkd3d_shader_cache *cache)
{
    if (cache->info.flags & (VKD3D_SHADER_CACHE_FLAGS_NO_SERIALIZE | VKD3D_SHADER_CACHE_FLAGS_READ_ONLY))
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

    if (cache->info.flags & VKD3D_SHADER_CACHE_FLAGS_READ_ONLY)
    {
        WARN("Attempt to modify a read-only cache.\n");
        return VKD3D_ERROR;
    }

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
    e->h.offset = 0;
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
