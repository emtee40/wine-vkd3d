/*
 * Copyright 2016 Józef Kucia for CodeWeavers
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

#ifndef __VKD3D_H
#define __VKD3D_H

#include <vkd3d_types.h>

#ifndef VKD3D_NO_WIN32_TYPES
# include <vkd3d_windows.h>
# include <vkd3d_d3d12.h>
#endif  /* VKD3D_NO_WIN32_TYPES */

#ifndef VKD3D_NO_VULKAN_H
# include <vulkan/vulkan.h>
#endif  /* VKD3D_NO_VULKAN_H */

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */

/**
 * \file vkd3d.h
 *
 * This file contains definitions for the vkd3d library.
 *
 * The vkd3d library is a 3D graphics library built on top of
 * Vulkan. It has an API very similar, but not identical, to
 * Direct3D 12.
 *
 * \since 1.0
 */

/** The type of a chained structure. */
enum vkd3d_structure_type
{
    /** The structure is a vkd3d_instance_create_info structure. */
    VKD3D_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
    /** The structure is a vkd3d_device_create_info structure. */
    VKD3D_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
    /** The structure is a vkd3d_image_resource_create_info structure. */
    VKD3D_STRUCTURE_TYPE_IMAGE_RESOURCE_CREATE_INFO,

    /**
     * The structure is a vkd3d_optional_instance_extensions_info structure.
     * \since 1.1
     */
    VKD3D_STRUCTURE_TYPE_OPTIONAL_INSTANCE_EXTENSIONS_INFO,

    /**
     * The structure is a vkd3d_optional_device_extensions_info structure.
     * \since 1.2
     */
    VKD3D_STRUCTURE_TYPE_OPTIONAL_DEVICE_EXTENSIONS_INFO,
    /**
     * The structure is a vkd3d_application_info structure.
     * \since 1.2
     */
    VKD3D_STRUCTURE_TYPE_APPLICATION_INFO,

    /**
     * The structure is a vkd3d_host_time_domain_info structure.
     * \since 1.3
     */
    VKD3D_STRUCTURE_TYPE_HOST_TIME_DOMAIN_INFO,

    VKD3D_FORCE_32_BIT_ENUM(VKD3D_STRUCTURE_TYPE),
};

enum vkd3d_api_version
{
    VKD3D_API_VERSION_1_0,
    VKD3D_API_VERSION_1_1,
    VKD3D_API_VERSION_1_2,
    VKD3D_API_VERSION_1_3,
    VKD3D_API_VERSION_1_4,
    VKD3D_API_VERSION_1_5,
    VKD3D_API_VERSION_1_6,
    VKD3D_API_VERSION_1_7,
    VKD3D_API_VERSION_1_8,
    VKD3D_API_VERSION_1_9,
    VKD3D_API_VERSION_1_10,
    VKD3D_API_VERSION_1_11,

    VKD3D_FORCE_32_BIT_ENUM(VKD3D_API_VERSION),
};

typedef HRESULT (*PFN_vkd3d_signal_event)(HANDLE event);

typedef void * (*PFN_vkd3d_thread)(void *data);

typedef void * (*PFN_vkd3d_create_thread)(PFN_vkd3d_thread thread_main, void *data);
typedef HRESULT (*PFN_vkd3d_join_thread)(void *thread);

struct vkd3d_instance;

/**
 * A chained structure containing instance creation parameters.
 */
struct vkd3d_instance_create_info
{
    /** Must be set to VKD3D_STRUCTURE_TYPE_INSTANCE_CREATE_INFO. */
    enum vkd3d_structure_type type;
    /** Optional pointer to a structure containing further parameters. */
    const void *next;

    /** An pointer to a function to signal events. */
    PFN_vkd3d_signal_event pfn_signal_event;
    /**
     * An optional pointer to a function to create threads. If this is NULL vkd3d will use a
     * function of its choice, depending on the platform. It must be NULL if and only if
     * pfn_join_thread is NULL.
     */
    PFN_vkd3d_create_thread pfn_create_thread;
    /**
     * An optional pointer to a function to join threads. If this is NULL vkd3d will use a
     * function of its choice, depending on the platform. It must be NULL if and only if
     * pfn_create_thread is NULL.
     */
    PFN_vkd3d_join_thread pfn_join_thread;
    /** The size of type WCHAR. It must be 2 or 4 and should normally be set to sizeof(WCHAR). */
    size_t wchar_size;

    /**
     * A pointer to the vkGetInstanceProcAddr Vulkan function, which will be used to load all the
     * other Vulkan functions. If set to NULL, vkd3d will search and use the Vulkan loader.
     */
    PFN_vkGetInstanceProcAddr pfn_vkGetInstanceProcAddr;

    /**
     * A list of Vulkan instance extensions to request. They are intended as required, so instance
     * creation will fail if any of them is not available.
     */
    const char * const *instance_extensions;
    /** The number of elements in the instance_extensions array. */
    uint32_t instance_extension_count;
};

/**
 * A chained structure to specify optional instance extensions.
 *
 * This structure extends vkd3d_instance_create_info.
 *
 * \since 1.1
 */
struct vkd3d_optional_instance_extensions_info
{
    /** Must be set to VKD3D_STRUCTURE_TYPE_OPTIONAL_INSTANCE_EXTENSIONS_INFO. */
    enum vkd3d_structure_type type;
    /** Optional pointer to a structure containing further parameters. */
    const void *next;

    /**
     * A list of optional Vulkan instance extensions to request. Instance creation does not fail if
     * they are not available.
     */
    const char * const *extensions;
    /** The number of elements in the extensions array. */
    uint32_t extension_count;
};

/**
 * A chained structure to specify application information.
 *
 * This structure extends vkd3d_instance_create_info.
 *
 * \since 1.2
 */
struct vkd3d_application_info
{
    /** Must be set to VKD3D_STRUCTURE_TYPE_APPLICATION_INFO. */
    enum vkd3d_structure_type type;
    /** Optional pointer to a structure containing further parameters. */
    const void *next;

    /**
     * The application's name, to be passed to the Vulkan implementation. If it is NULL, a name is
     * computed from the process executable filename. If that cannot be done, the empty string is
     * used.
     */
    const char *application_name;
    /** The application's version, to be passed to the Vulkan implementation. */
    uint32_t application_version;

    /**
     * The engine name, to be passed to the Vulkan implementation. If it is NULL, "vkd3d" is used.
     */
    const char *engine_name;
    /**
     * The engine version, to be passed to the Vulkan implementation. If it is 0, the version is
     * computed from the vkd3d library version.
     */
    uint32_t engine_version;

    /**
     * The vkd3d API version to use, to guarantee backward compatibility of the shared library. If
     * this chained structure is not used then VKD3D_API_VERSION_1_0 is used.
     */
    enum vkd3d_api_version api_version;
};

/* Extends vkd3d_instance_create_info. Available since 1.3. */
struct vkd3d_host_time_domain_info
{
    enum vkd3d_structure_type type;
    const void *next;

    uint64_t ticks_per_second;
};

struct vkd3d_device_create_info
{
    enum vkd3d_structure_type type;
    const void *next;

    D3D_FEATURE_LEVEL minimum_feature_level;

    struct vkd3d_instance *instance;
    const struct vkd3d_instance_create_info *instance_create_info;

    VkPhysicalDevice vk_physical_device;

    const char * const *device_extensions;
    uint32_t device_extension_count;

    IUnknown *parent;
    LUID adapter_luid;
};

/* Extends vkd3d_device_create_info. Available since 1.2. */
struct vkd3d_optional_device_extensions_info
{
    enum vkd3d_structure_type type;
    const void *next;

    const char * const *extensions;
    uint32_t extension_count;
};

/* vkd3d_image_resource_create_info flags */
#define VKD3D_RESOURCE_INITIAL_STATE_TRANSITION 0x00000001
#define VKD3D_RESOURCE_PRESENT_STATE_TRANSITION 0x00000002

struct vkd3d_image_resource_create_info
{
    enum vkd3d_structure_type type;
    const void *next;

    VkImage vk_image;
    D3D12_RESOURCE_DESC desc;
    unsigned int flags;
    D3D12_RESOURCE_STATES present_state;
};

#ifdef LIBVKD3D_SOURCE
# define VKD3D_API VKD3D_EXPORT
#else
# define VKD3D_API VKD3D_IMPORT
#endif

#ifndef VKD3D_NO_PROTOTYPES

VKD3D_API HRESULT vkd3d_create_instance(const struct vkd3d_instance_create_info *create_info,
        struct vkd3d_instance **instance);
VKD3D_API ULONG vkd3d_instance_decref(struct vkd3d_instance *instance);
VKD3D_API VkInstance vkd3d_instance_get_vk_instance(struct vkd3d_instance *instance);
VKD3D_API ULONG vkd3d_instance_incref(struct vkd3d_instance *instance);

VKD3D_API HRESULT vkd3d_create_device(const struct vkd3d_device_create_info *create_info,
        REFIID iid, void **device);
VKD3D_API IUnknown *vkd3d_get_device_parent(ID3D12Device *device);
VKD3D_API VkDevice vkd3d_get_vk_device(ID3D12Device *device);
VKD3D_API VkPhysicalDevice vkd3d_get_vk_physical_device(ID3D12Device *device);
VKD3D_API struct vkd3d_instance *vkd3d_instance_from_device(ID3D12Device *device);

VKD3D_API uint32_t vkd3d_get_vk_queue_family_index(ID3D12CommandQueue *queue);

/**
 * Acquire the Vulkan queue backing a command queue.
 *
 * While a queue is acquired by the client, it is locked so that
 * neither the vkd3d library nor other threads can submit work to
 * it. For that reason it should be released as soon as possible with
 * vkd3d_release_vk_queue(). The lock is not reentrant, so the same
 * queue must not be acquired more than once by the same thread.
 *
 * Work submitted through the Direct3D 12 API exposed by vkd3d is not
 * always immediately submitted to the Vulkan queue; sometimes it is
 * kept in another internal queue, which might not necessarily be
 * empty at the time vkd3d_acquire_vk_queue() is called. For this
 * reason, work submitted directly to the Vulkan queue might appear to
 * the Vulkan driver as being submitted before other work submitted
 * though the Direct3D 12 API. If this is not desired, it is
 * recommended to synchronize work submission using an ID3D12Fence
 * object, by submitting to the queue a signal operation after all the
 * Direct3D 12 work is submitted and waiting for it before calling
 * vkd3d_acquire_vk_queue().
 *
 * \since 1.0
 */
VKD3D_API VkQueue vkd3d_acquire_vk_queue(ID3D12CommandQueue *queue);

/**
 * Release the Vulkan queue backing a command queue.
 *
 * This must be paired to an earlier corresponding
 * vkd3d_acquire_vk_queue(). After this function is called, the Vulkan
 * queue returned by vkd3d_acquire_vk_queue() must not be used any
 * more.
 *
 * \since 1.0
 */
VKD3D_API void vkd3d_release_vk_queue(ID3D12CommandQueue *queue);

VKD3D_API HRESULT vkd3d_create_image_resource(ID3D12Device *device,
        const struct vkd3d_image_resource_create_info *create_info, ID3D12Resource **resource);
VKD3D_API ULONG vkd3d_resource_decref(ID3D12Resource *resource);
VKD3D_API ULONG vkd3d_resource_incref(ID3D12Resource *resource);

VKD3D_API HRESULT vkd3d_serialize_root_signature(const D3D12_ROOT_SIGNATURE_DESC *desc,
        D3D_ROOT_SIGNATURE_VERSION version, ID3DBlob **blob, ID3DBlob **error_blob);
VKD3D_API HRESULT vkd3d_create_root_signature_deserializer(const void *data, SIZE_T data_size,
        REFIID iid, void **deserializer);

VKD3D_API VkFormat vkd3d_get_vk_format(DXGI_FORMAT format);

/* 1.1 */
VKD3D_API DXGI_FORMAT vkd3d_get_dxgi_format(VkFormat format);

/* 1.2 */
VKD3D_API HRESULT vkd3d_serialize_versioned_root_signature(const D3D12_VERSIONED_ROOT_SIGNATURE_DESC *desc,
        ID3DBlob **blob, ID3DBlob **error_blob);
VKD3D_API HRESULT vkd3d_create_versioned_root_signature_deserializer(const void *data, SIZE_T data_size,
        REFIID iid, void **deserializer);

/**
 * Set a callback to be called when vkd3d outputs debug logging.
 *
 * If NULL, or if this function has not been called, libvkd3d will print all
 * enabled log output to stderr.
 *
 * Calling this function will also set the log callback for libvkd3d-shader.
 *
 * \param callback Callback function to set.
 *
 * \since 1.4
 */
VKD3D_API void vkd3d_set_log_callback(PFN_vkd3d_log callback);

#endif  /* VKD3D_NO_PROTOTYPES */

/*
 * Function pointer typedefs for vkd3d functions.
 */
typedef HRESULT (*PFN_vkd3d_create_instance)(const struct vkd3d_instance_create_info *create_info,
        struct vkd3d_instance **instance);
typedef ULONG (*PFN_vkd3d_instance_decref)(struct vkd3d_instance *instance);
typedef VkInstance (*PFN_vkd3d_instance_get_vk_instance)(struct vkd3d_instance *instance);
typedef ULONG (*PFN_vkd3d_instance_incref)(struct vkd3d_instance *instance);

typedef HRESULT (*PFN_vkd3d_create_device)(const struct vkd3d_device_create_info *create_info,
        REFIID iid, void **device);
typedef IUnknown * (*PFN_vkd3d_get_device_parent)(ID3D12Device *device);
typedef VkDevice (*PFN_vkd3d_get_vk_device)(ID3D12Device *device);
typedef VkPhysicalDevice (*PFN_vkd3d_get_vk_physical_device)(ID3D12Device *device);
typedef struct vkd3d_instance * (*PFN_vkd3d_instance_from_device)(ID3D12Device *device);

typedef uint32_t (*PFN_vkd3d_get_vk_queue_family_index)(ID3D12CommandQueue *queue);
typedef VkQueue (*PFN_vkd3d_acquire_vk_queue)(ID3D12CommandQueue *queue);
typedef void (*PFN_vkd3d_release_vk_queue)(ID3D12CommandQueue *queue);

typedef HRESULT (*PFN_vkd3d_create_image_resource)(ID3D12Device *device,
        const struct vkd3d_image_resource_create_info *create_info, ID3D12Resource **resource);
typedef ULONG (*PFN_vkd3d_resource_decref)(ID3D12Resource *resource);
typedef ULONG (*PFN_vkd3d_resource_incref)(ID3D12Resource *resource);

typedef HRESULT (*PFN_vkd3d_serialize_root_signature)(const D3D12_ROOT_SIGNATURE_DESC *desc,
        D3D_ROOT_SIGNATURE_VERSION version, ID3DBlob **blob, ID3DBlob **error_blob);
typedef HRESULT (*PFN_vkd3d_create_root_signature_deserializer)(const void *data, SIZE_T data_size,
        REFIID iid, void **deserializer);

typedef VkFormat (*PFN_vkd3d_get_vk_format)(DXGI_FORMAT format);

/* 1.1 */
typedef DXGI_FORMAT (*PFN_vkd3d_get_dxgi_format)(VkFormat format);

/* 1.2 */
typedef HRESULT (*PFN_vkd3d_serialize_versioned_root_signature)(const D3D12_VERSIONED_ROOT_SIGNATURE_DESC *desc,
        ID3DBlob **blob, ID3DBlob **error_blob);
typedef HRESULT (*PFN_vkd3d_create_versioned_root_signature_deserializer)(const void *data, SIZE_T data_size,
        REFIID iid, void **deserializer);

/** Type of vkd3d_set_log_callback(). \since 1.4 */
typedef void (*PFN_vkd3d_set_log_callback)(PFN_vkd3d_log callback);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* __VKD3D_H */
