/*
 * Copyright 2024 Feifan He for CodeWeavers
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

#include "config.h"
#import <Metal/Metal.h>
#define VKD3D_TEST_NO_DEFS
/* Avoid conflicts with the Objective C BOOL definition. */
#define BOOL VKD3D_BOOLEAN
#include "shader_runner.h"
#undef BOOL

static const MTLResourceOptions DEFAULT_BUFFER_RESOURCE_OPTION = MTLResourceCPUCacheModeDefaultCache
        | MTLResourceStorageModeShared 
        | MTLResourceHazardTrackingModeDefault;

struct metal_resource
{
    struct resource r;

    id<MTLBuffer> buffer;
    id<MTLTexture> texture;
};

struct metal_resource_readback
{
    struct resource_readback rb;
    id<MTLBuffer> buffer;
};

static MTLPixelFormat get_metal_pixel_format(DXGI_FORMAT format)
{
    switch (format)
    {
        case DXGI_FORMAT_R32G32B32A32_FLOAT:
            return MTLPixelFormatRGBA32Float;
        case DXGI_FORMAT_R32G32B32A32_UINT:
            return MTLPixelFormatRGBA32Uint;
        case DXGI_FORMAT_R32G32B32A32_SINT:
            return MTLPixelFormatRGBA32Sint;
        case DXGI_FORMAT_R32G32_FLOAT:
            return MTLPixelFormatRG32Float;
        case DXGI_FORMAT_R32G32_UINT:
            return MTLPixelFormatRG32Uint;
        case DXGI_FORMAT_R32G32_SINT:
            return MTLPixelFormatRG32Sint;
        case DXGI_FORMAT_R32_FLOAT:
            return MTLPixelFormatR32Float;
        case DXGI_FORMAT_R32_TYPELESS:
        case DXGI_FORMAT_R32_UINT:
            return MTLPixelFormatR32Uint;
        case DXGI_FORMAT_R32_SINT:
            return MTLPixelFormatR32Sint;
        case DXGI_FORMAT_D32_FLOAT:
            return MTLPixelFormatDepth32Float;
        default:
            return MTLPixelFormatInvalid;
    }
};

static unsigned int get_metal_format_texel_size(DXGI_FORMAT format)
{
    switch (format)
    {
        case DXGI_FORMAT_R32G32B32A32_UINT:
        case DXGI_FORMAT_R32G32B32A32_SINT:
        case DXGI_FORMAT_R32G32B32A32_FLOAT:
            return 16;
        case DXGI_FORMAT_R32G32_UINT:
        case DXGI_FORMAT_R32G32_SINT:
        case DXGI_FORMAT_R32G32_FLOAT:
            return 8;
        case DXGI_FORMAT_R32_TYPELESS:
        case DXGI_FORMAT_R32_UINT:
        case DXGI_FORMAT_R32_SINT:
        case DXGI_FORMAT_R32_FLOAT:
        case DXGI_FORMAT_D32_FLOAT:
            return 4;
        default:
            return 0;
    }
};

static struct metal_resource *metal_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct metal_resource, r);
}

struct metal_runner
{
    struct shader_runner r;
    struct shader_runner_caps caps;

    id<MTLDevice> device;
    id<MTLCommandQueue> queue;
};

static struct metal_runner *metal_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct metal_runner, r);
}

static void init_resource_buffer(struct metal_runner *runner, struct metal_resource *resource,
        const struct resource_params *params)
{
    unsigned int texel_size = get_metal_format_texel_size(params->desc.format);
    MTLPixelFormat format = get_metal_pixel_format(params->desc.format);
    id<MTLDevice> device = runner->device;

    if (params->data)
        resource->buffer = [device newBufferWithBytes:params->data
                length:params->data_size
                options:DEFAULT_BUFFER_RESOURCE_OPTION];
    else
        resource->buffer = [device newBufferWithLength:params->data_size options:DEFAULT_BUFFER_RESOURCE_OPTION];

    /* create a R32Uint view for structured buffer */
    if (params->stride && format == MTLPixelFormatInvalid)
    {
        format = MTLPixelFormatR32Uint;
        texel_size = 4;
    }

    ok(format != MTLPixelFormatInvalid, "Unhandled pixel format %u.\n", params->desc.format);

    if (format != MTLPixelFormatInvalid && texel_size > 0)
    {
        MTLTextureDescriptor *desc = [[MTLTextureDescriptor alloc] init];

        desc.textureType = MTLTextureTypeTextureBuffer;
        desc.pixelFormat = format;
        desc.width = params->data_size / texel_size;

        resource->texture = [resource->buffer newTextureWithDescriptor:desc offset:0 bytesPerRow:params->data_size];

        [desc release];
    }
}

static void init_resource_2d(struct metal_runner *runner, struct metal_resource *resource,
        const struct resource_params *params)
{
    id<MTLDevice> device = runner->device;
    MTLTextureDescriptor *desc;

    if (params->desc.sample_count > 1)
    {
        if (params->desc.level_count > 1)
            fatal_error("Multisampled texture has multiple levels.\n");

        if (![device supportsTextureSampleCount:params->desc.sample_count])
        {
            trace("Format #%x with sample count %u is not supported; skipping.\n", params->desc.format,
                    params->desc.sample_count);
            return;
        }
    }

    desc = [[MTLTextureDescriptor alloc] init];

    desc.width = params->desc.width;
    desc.height = params->desc.height;
    desc.mipmapLevelCount = params->desc.level_count;
    desc.arrayLength = 1;
    desc.pixelFormat = get_metal_pixel_format(params->desc.format);
    ok(desc.pixelFormat != MTLPixelFormatInvalid, "Unhandled pixel format %u.\n", params->desc.format);
    if(desc.pixelFormat == MTLPixelFormatInvalid)
    {
        [desc release];
        return;
    }
    desc.sampleCount = max(params->desc.sample_count, 1);
    if(desc.sampleCount > 1)
        desc.textureType = MTLTextureType2DMultisample;
    if (params->desc.type == RESOURCE_TYPE_UAV)
        desc.usage = MTLTextureUsageShaderRead | MTLTextureUsageShaderWrite;
    else if (params->desc.type == RESOURCE_TYPE_RENDER_TARGET)
        desc.usage = MTLTextureUsageRenderTarget;
    else if (params->desc.type == RESOURCE_TYPE_DEPTH_STENCIL)
        desc.usage = MTLTextureUsageRenderTarget;
    else
        desc.usage = MTLTextureUsageShaderRead;

    resource->texture = [device newTextureWithDescriptor:desc];

    ok(resource->texture, "Failed to create texture.\n");

    [desc release];

    if (params->data)
    {
        unsigned int buffer_offset = 0;

        if (params->desc.sample_count > 1)
            fatal_error("Cannot upload data to a multisampled texture.\n");

        for (unsigned int level = 0; level < params->desc.level_count; ++level)
        {
            unsigned int level_width = get_level_dimension(params->desc.width, level);
            unsigned int level_height = get_level_dimension(params->desc.height, level);

            [resource->texture replaceRegion:MTLRegionMake2D(0, 0, level_width, level_height)
                    mipmapLevel:level
                    slice:0
                    withBytes:&params->data[buffer_offset]
                    bytesPerRow:level_width * params->desc.texel_size
                    bytesPerImage:level_height * level_width * params->desc.texel_size];
            buffer_offset += level_height * level_width * params->desc.texel_size;
        }
    }
}

static struct resource *metal_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct metal_runner *runner = metal_runner(r);
    id<MTLDevice> device = runner->device;
    struct metal_resource *resource;

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    switch (params->desc.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
        case RESOURCE_TYPE_DEPTH_STENCIL:
        case RESOURCE_TYPE_TEXTURE:
        case RESOURCE_TYPE_UAV:
            if (params->desc.dimension == RESOURCE_DIMENSION_BUFFER)
                init_resource_buffer(runner, resource, params);
            else
                init_resource_2d(runner, resource, params);
            break;
        case RESOURCE_TYPE_VERTEX_BUFFER:
            if (params->data)
                resource->buffer = [device newBufferWithBytes:params->data
                        length:params->data_size
                        options:DEFAULT_BUFFER_RESOURCE_OPTION];
            else
                resource->buffer = [device newBufferWithLength:params->data_size
                        options:DEFAULT_BUFFER_RESOURCE_OPTION];
            break;
    }

    return &resource->r;
};

static void metal_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct metal_resource *resource = metal_resource(res);

    if (resource->buffer)
        [resource->buffer release];
    if (resource->texture)
        [resource->texture release];
    free(resource);
};

static bool metal_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    return false;
};

static void metal_runner_clear(struct shader_runner *r, struct resource *res, const struct vec4 *clear_value)
{
    struct metal_resource *resource = metal_resource(res);
    struct metal_runner *runner = metal_runner(r);

    @autoreleasepool
    {
        MTLRenderPassDescriptor *descriptor = [MTLRenderPassDescriptor renderPassDescriptor];
        id<MTLCommandBuffer> cmdbuf = [runner->queue commandBuffer];
        id<MTLRenderCommandEncoder> encoder;

        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
                descriptor.colorAttachments[0].texture = resource->texture;
                descriptor.colorAttachments[0].loadAction = MTLLoadActionClear;
                descriptor.colorAttachments[0].storeAction = MTLStoreActionStore;
                descriptor.colorAttachments[0].clearColor =
                        MTLClearColorMake(clear_value->x, clear_value->y, clear_value->z, clear_value->w);
                break;
            case RESOURCE_TYPE_DEPTH_STENCIL:
                descriptor.depthAttachment.texture = resource->texture;
                descriptor.depthAttachment.loadAction = MTLLoadActionClear;
                descriptor.depthAttachment.storeAction = MTLStoreActionStore;
                descriptor.depthAttachment.clearDepth = clear_value->x;
                break;
            default:
                fatal_error("Clears are not implemented for resource type %u.\n", resource->r.desc.type);
        }
        encoder = [cmdbuf renderCommandEncoderWithDescriptor:descriptor];
        [encoder endEncoding];

        [cmdbuf commit];
        [cmdbuf waitUntilCompleted];
    }
};

static bool metal_runner_draw(struct shader_runner *r, D3D_PRIMITIVE_TOPOLOGY primitive_topology,
        unsigned int vertex_count, unsigned int instance_count)
{
    return false;
};

static bool metal_runner_copy(struct shader_runner *r, struct resource *src, struct resource *dst)
{
    return false;
};

static struct resource_readback *metal_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct metal_resource_readback *rb = malloc(sizeof(*rb));
    struct metal_resource *resource = metal_resource(res);
    struct metal_runner *runner = metal_runner(r);

    rb->rb.width = resource->r.desc.width;
    rb->rb.height = resource->r.desc.height;
    rb->rb.depth = 1;
    rb->rb.row_pitch = rb->rb.width * resource->r.desc.texel_size;
    rb->buffer = [runner->device newBufferWithLength:rb->rb.row_pitch * rb->rb.height
            options:DEFAULT_BUFFER_RESOURCE_OPTION];

    @autoreleasepool
    {
        id<MTLBlitCommandEncoder> blit;
        id<MTLCommandBuffer> cmdbuf;

        cmdbuf = [runner->queue commandBuffer];
        blit = [cmdbuf blitCommandEncoder];

        if (resource->r.desc.dimension == RESOURCE_DIMENSION_BUFFER)
        {
            [blit copyFromBuffer:resource->buffer
                    sourceOffset:0
                    toBuffer:rb->buffer
                    destinationOffset:0
                    size:rb->rb.row_pitch * rb->rb.height];
        }
        else
        {
            if (resource->r.desc.sample_count > 1)
            {
                /* TODO: multisampeld texture readback */
            }
            else
            {
                [blit copyFromTexture:resource->texture
                        sourceSlice:0
                        sourceLevel:0
                        sourceOrigin:MTLOriginMake(0, 0, 0)
                        sourceSize:MTLSizeMake(rb->rb.width, rb->rb.height, rb->rb.depth)
                        toBuffer:rb->buffer
                        destinationOffset:0
                        destinationBytesPerRow:rb->rb.row_pitch
                        destinationBytesPerImage:0];
            }
        }
        [blit endEncoding];

        [cmdbuf commit];
        [cmdbuf waitUntilCompleted];
    }

    rb->rb.data = rb->buffer.contents;
    return &rb->rb;
}

static void metal_runner_release_readback(struct shader_runner *r, struct resource_readback *rb)
{
    struct metal_resource_readback *metal_rb = CONTAINING_RECORD(rb, struct metal_resource_readback, rb);

    [metal_rb->buffer release];
    free(rb);
}

static const struct shader_runner_ops metal_runner_ops =
{
    .create_resource = metal_runner_create_resource,
    .destroy_resource = metal_runner_destroy_resource,
    .dispatch = metal_runner_dispatch,
    .clear = metal_runner_clear,
    .draw = metal_runner_draw,
    .copy = metal_runner_copy,
    .get_resource_readback = metal_runner_get_resource_readback,
    .release_readback = metal_runner_release_readback,
};

static bool metal_runner_init(struct metal_runner *runner)
{
    NSArray<id<MTLDevice>> *devices;
    id<MTLCommandQueue> queue;
    id<MTLDevice> device;

    static const char *const tags[] =
    {
        "msl",
    };

    memset(runner, 0, sizeof(*runner));

    devices = MTLCopyAllDevices();
    if (![devices count])
    {
        skip("Failed to find a usable Metal device.\n");
        [devices release];
        return false;
    }
    device = [devices objectAtIndex:0];
    [devices release];
    queue = [device newCommandQueue];
    if (!queue)
    {
        skip("Failed to create command queue.\n");
        [device release];
        return false;
    }

    trace("GPU: %s\n", [[device name] UTF8String]);

    runner->device = device;
    runner->queue = queue;

    runner->caps.runner = "Metal";
    runner->caps.tags = tags;
    runner->caps.tag_count = ARRAY_SIZE(tags);
    runner->caps.minimum_shader_model = SHADER_MODEL_4_0;
    runner->caps.maximum_shader_model = SHADER_MODEL_5_0;

    return true;
}

static void metal_runner_cleanup(struct metal_runner *runner)
{
    [runner->queue release];
    runner->queue = nil;
    [runner->device release];
    runner->device = nil;
}

void run_shader_tests_metal(void)
{
    struct metal_runner runner;

    if (!metal_runner_init(&runner))
        return;
    run_shader_tests(&runner.r, &runner.caps, &metal_runner_ops, NULL);
    metal_runner_cleanup(&runner);
}
