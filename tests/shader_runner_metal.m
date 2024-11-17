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
#define COBJMACROS
#define VKD3D_TEST_NO_DEFS
/* Avoid conflicts with the Objective C BOOL definition. */
#define BOOL VKD3D_BOOLEAN
#include "shader_runner.h"
#include "vkd3d_d3dcommon.h"
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

static MTLVertexFormat get_metal_attribute_format(DXGI_FORMAT format)
{
    switch (format)
    {
        case DXGI_FORMAT_R32G32B32A32_FLOAT:
            return MTLVertexFormatFloat4;
        case DXGI_FORMAT_R32G32B32A32_UINT:
            return MTLVertexFormatUInt4;
        case DXGI_FORMAT_R32G32B32A32_SINT:
            return MTLVertexFormatInt4;
        case DXGI_FORMAT_R32G32_FLOAT:
            return MTLVertexFormatFloat2;
        case DXGI_FORMAT_R32G32_UINT:
            return MTLVertexFormatUInt2;
        case DXGI_FORMAT_R32G32_SINT:
            return MTLVertexFormatInt2;
        case DXGI_FORMAT_R32_FLOAT:
            return MTLVertexFormatFloat;
        case DXGI_FORMAT_R32_UINT:
            return MTLVertexFormatUInt;
        case DXGI_FORMAT_R32_SINT:
            return MTLVertexFormatInt;
        default:
            return MTLVertexFormatInvalid;
    }
};

static bool get_metal_primitive_type(D3D_PRIMITIVE_TOPOLOGY primitive_topology, MTLPrimitiveType *primitive_type)
{
    switch (primitive_topology)
    {
        case D3D_PRIMITIVE_TOPOLOGY_POINTLIST:
            *primitive_type = MTLPrimitiveTypePoint;
            break;
        case D3D_PRIMITIVE_TOPOLOGY_LINELIST:
            *primitive_type = MTLPrimitiveTypeLine;
            break;
        case D3D_PRIMITIVE_TOPOLOGY_LINESTRIP:
            *primitive_type = MTLPrimitiveTypeLineStrip;
            break;
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST:
            *primitive_type = MTLPrimitiveTypeTriangle;
            break;
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP:
            *primitive_type = MTLPrimitiveTypeTriangleStrip;
            break;
        default:
            return false;
    }
    return true;
}

static MTLCompareFunction get_metal_compare_function(D3D12_COMPARISON_FUNC func)
{
    switch (func)
    {
        case VKD3D_SHADER_COMPARISON_FUNC_NEVER:
            return MTLCompareFunctionNever;
        case VKD3D_SHADER_COMPARISON_FUNC_LESS:
            return MTLCompareFunctionLess;
        case VKD3D_SHADER_COMPARISON_FUNC_EQUAL:
            return MTLCompareFunctionEqual;
        case VKD3D_SHADER_COMPARISON_FUNC_LESS_EQUAL:
            return MTLCompareFunctionLessEqual;
        case VKD3D_SHADER_COMPARISON_FUNC_GREATER:
            return MTLCompareFunctionGreater;
        case VKD3D_SHADER_COMPARISON_FUNC_NOT_EQUAL:
            return MTLCompareFunctionNotEqual;
        case VKD3D_SHADER_COMPARISON_FUNC_GREATER_EQUAL:
            return MTLCompareFunctionGreaterEqual;
        case VKD3D_SHADER_COMPARISON_FUNC_ALWAYS:
            return MTLCompareFunctionAlways;
    }
}

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

    ID3D10Blob *d3d_blobs[SHADER_TYPE_COUNT];
    struct vkd3d_shader_scan_signature_info signatures[SHADER_TYPE_COUNT];
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
            skip("Format #%x with sample count %u is not supported; skipping.\n", params->desc.format,
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

static bool compile_shader(struct metal_runner *runner, enum shader_type type, struct vkd3d_shader_code *out)
{
    struct vkd3d_shader_interface_info interface_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_INTERFACE_INFO};
    struct vkd3d_shader_compile_info info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_resource_binding bindings[MAX_RESOURCES + MAX_SAMPLERS];
    struct vkd3d_shader_compile_option options[3];
    struct vkd3d_shader_resource_binding *binding;
    struct vkd3d_shader_compile_option *option;
    unsigned int descriptor_binding = 0;
    bool ret = true;
    unsigned int i;
    char *messages;

    if(!(runner->d3d_blobs[type] = compile_hlsl(&runner->r, type)))
        return false;

    info.next = &interface_info;
    info.source.code = ID3D10Blob_GetBufferPointer(runner->d3d_blobs[type]);
    info.source.size = ID3D10Blob_GetBufferSize(runner->d3d_blobs[type]);

    if (runner->r.minimum_shader_model < SHADER_MODEL_4_0)
        info.source_type = VKD3D_SHADER_SOURCE_D3D_BYTECODE;
    else
        info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;
    info.target_type = VKD3D_SHADER_TARGET_MSL;

    info.options = options;
    info.option_count = 0;

    option = &options[info.option_count++];
    option->name = VKD3D_SHADER_COMPILE_OPTION_FEATURE;
    option->value = shader_runner_caps_get_feature_flags(&runner->caps);

    if (runner->r.uniform_count)
    {
        binding = &bindings[interface_info.binding_count++];
        binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_CBV;
        binding->register_space = 0;
        binding->register_index = 0;
        binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
        binding->flags = VKD3D_SHADER_BINDING_FLAG_BUFFER;
        binding->binding.set = 0;
        binding->binding.binding = descriptor_binding++;
        binding->binding.count = 1;
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct metal_resource *resource = metal_resource(runner->r.resources[i]);

        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;

            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                skip("Unhandled texture/uav resource.\n");
                ret = false;
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        skip("Unhandled sampler state.\n");
        ret = false;
    }

    interface_info.bindings = bindings;
    interface_info.next = &runner->signatures[type];
    runner->signatures[type].type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_SIGNATURE_INFO;
    runner->signatures[type].next = NULL;

    /* vkd3d_shader_compile always executes no matter what value `ret` is */
    ret = (vkd3d_shader_compile(&info, out, &messages) >= 0) && ret;
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);

    return ret;
}

static id<MTLFunction> compile_stage(struct metal_runner *runner, enum shader_type type)
{
    struct vkd3d_shader_code out;
    id<MTLFunction> function;
    id<MTLLibrary> library;
    NSString *src;
    NSError *err;

    if (!compile_shader(runner, type, &out))
        return nil;
    src = [[[NSString alloc] initWithBytes:out.code length:out.size encoding:NSUTF8StringEncoding] autorelease];
    library = [[runner->device newLibraryWithSource:src options:nil error:&err] autorelease];
    ok(library, "Failed to create MTLLibrary: %s.\n", [err.description cStringUsingEncoding:NSUTF8StringEncoding]); 
    function = [library newFunctionWithName:@"shader_entry"];
    ok(function, "Failed to create MTLFunction.\n");
    vkd3d_shader_free_shader_code(&out);
    return [function autorelease];
}

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
    MTLViewport viewport = {0, 0, RENDER_TARGET_WIDTH, RENDER_TARGET_HEIGHT, 0.0f, 1.0f};
    struct metal_runner *runner = metal_runner(r);
    /* [[buffer(0)]] used by descriptor argument buffer */
    unsigned int vertex_buffer_binding_offset = 1;
    id<MTLArgumentEncoder> vs_argenc = nil;
    id<MTLArgumentEncoder> ps_argenc = nil;
    id<MTLDevice> device = runner->device;
    MTLPrimitiveType primitive_type;
    id<MTLRenderPipelineState> pso;
    id<MTLTexture> rtvs[8] = {0};
    unsigned int rtv_count = 0;
    id<MTLBuffer> argbuf = nil;
    id<MTLTexture> dsv = nil;
    id<MTLBuffer> cb = nil;
    NSError* err = nil;
    bool ret = false;
    unsigned int i;

    if (!get_metal_primitive_type(primitive_topology, &primitive_type))
    {
        skip("Unhandled primitive topology %u.\n", primitive_topology);
        return false;
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct metal_resource *resource = metal_resource(runner->r.resources[i]);
        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
                rtvs[resource->r.desc.slot] = resource->texture;
                rtv_count = max(rtv_count, resource->r.desc.slot + 1);
                break;

            case RESOURCE_TYPE_DEPTH_STENCIL:
                dsv = resource->texture;
                break;
            default:
                break;
        }
    }

    @autoreleasepool
    {
        MTLRenderPassDescriptor *pass_desc = [MTLRenderPassDescriptor renderPassDescriptor];
        id<MTLCommandBuffer> cmdbuf = [runner->queue commandBuffer];
        MTLRenderPipelineDescriptor *pipeline_desc;
        id<MTLRenderCommandEncoder> encoder;

        for (i = 0; i < rtv_count; i++)
        {
            MTLRenderPassColorAttachmentDescriptor *attachment;
            if (!rtvs[i])
                continue;
            attachment = pass_desc.colorAttachments[i];
            attachment.loadAction = MTLLoadActionLoad;
            attachment.storeAction = MTLStoreActionStore;
            attachment.texture = rtvs[i];
        }
        if (dsv)
        {
            pass_desc.depthAttachment.texture = dsv;
            pass_desc.depthAttachment.loadAction = MTLLoadActionLoad;
            pass_desc.depthAttachment.storeAction = MTLStoreActionStore;
        }

        encoder = [cmdbuf renderCommandEncoderWithDescriptor:pass_desc];

        pipeline_desc = [[[MTLRenderPipelineDescriptor alloc] init] autorelease];

        for (i = 0; i < runner->r.resource_count; ++i)
        {
            struct metal_resource *resource = metal_resource(runner->r.resources[i]);

            switch (resource->r.desc.type)
            {
                case RESOURCE_TYPE_RENDER_TARGET:
                    pipeline_desc.colorAttachments[resource->r.desc.slot].pixelFormat = resource->texture.pixelFormat;
                    break;
                case RESOURCE_TYPE_DEPTH_STENCIL:
                {
                    MTLDepthStencilDescriptor *ds_desc;
                    id<MTLDepthStencilState> ds_state;
                    pipeline_desc.depthAttachmentPixelFormat = resource->texture.pixelFormat;
                    ds_desc = [[MTLDepthStencilDescriptor alloc] init];
                    ds_desc.depthCompareFunction = get_metal_compare_function(runner->r.depth_func);
                    ds_desc.depthWriteEnabled = true;
                    ds_state = [device newDepthStencilStateWithDescriptor:ds_desc];
                    [encoder setDepthStencilState:ds_state];
                    [ds_state autorelease];
                    break;
                }
                case RESOURCE_TYPE_TEXTURE:
                case RESOURCE_TYPE_UAV:
                case RESOURCE_TYPE_VERTEX_BUFFER:
                    break;
            }
        }

        if (!(pipeline_desc.vertexFunction = compile_stage(runner, SHADER_TYPE_VS)))
            goto out;

        if (!(pipeline_desc.fragmentFunction = compile_stage(runner, SHADER_TYPE_PS)))
            goto out;

        if (runner->r.shader_source[SHADER_TYPE_HS])
        {
            skip("Tessellation is not implemented in Metal runner.\n");
            goto out;
        }

        if (runner->r.shader_source[SHADER_TYPE_GS])
        {
            skip("Geometry shader is not implemented in Metal runner.\n");
            goto out;
        }

        vs_argenc = [[pipeline_desc.vertexFunction newArgumentEncoderWithBufferIndex:0] autorelease];
        ps_argenc = [[pipeline_desc.fragmentFunction newArgumentEncoderWithBufferIndex:0] autorelease];

        if (runner->r.input_element_count)
        {
            MTLVertexDescriptor *vertex_desc = [MTLVertexDescriptor vertexDescriptor];

            if (runner->r.input_element_count > 32)
                fatal_error("Input element count %zu is too high.\n", runner->r.input_element_count);

            for (i = 0; i < runner->r.input_element_count; ++i)
            {
                const struct input_element *element = &runner->r.input_elements[i];
                const struct vkd3d_shader_signature_element *signature_element;
                MTLVertexAttributeDescriptor *attribute;

                signature_element = vkd3d_shader_find_signature_element(&runner->signatures[SHADER_TYPE_VS].input, element->name,
                        element->index, 0);
                ok(signature_element, "Cannot find signature element %s%u.\n", element->name, element->index);

                attribute = [vertex_desc.attributes objectAtIndexedSubscript:signature_element->register_index];
                attribute.bufferIndex = element->slot + vertex_buffer_binding_offset;
                attribute.format = get_metal_attribute_format(element->format);
                ok(attribute.format != MTLVertexFormatInvalid, "Unhandled attribute format %u.\n", element->format);
            }
            for (i = 0; i < runner->r.resource_count; ++i)
            {
                const struct metal_resource *resource = metal_resource(runner->r.resources[i]);
                switch (resource->r.desc.type)
                {
                    case RESOURCE_TYPE_VERTEX_BUFFER:
                    {
                        unsigned int j;
                        MTLVertexBufferLayoutDescriptor *binding = [vertex_desc.layouts
                                objectAtIndexedSubscript:resource->r.desc.slot + vertex_buffer_binding_offset];
                        binding.stepFunction = MTLVertexStepFunctionPerVertex;
                        binding.stride = 0;
                        for (j = 0; j < runner->r.input_element_count; ++j)
                        {
                            if (runner->r.input_elements[j].slot == resource->r.desc.slot)
                            {
                                const struct vkd3d_shader_signature_element *signature_element;
                                MTLVertexAttributeDescriptor *attribute;
                                signature_element = vkd3d_shader_find_signature_element(
                                        &runner->signatures[SHADER_TYPE_VS].input,
                                        runner->r.input_elements[j].name, runner->r.input_elements[j].index, 0);
                                attribute = [vertex_desc.attributes
                                        objectAtIndexedSubscript:signature_element->register_index];
                                attribute.offset = binding.stride;
                                binding.stride += runner->r.input_elements[j].texel_size;
                            }
                        }
                        [encoder setVertexBuffer:resource->buffer
                                offset:0
                                atIndex:resource->r.desc.slot + vertex_buffer_binding_offset];
                        break;
                    }
                    default:
                        break;
                }
            }
            pipeline_desc.vertexDescriptor = vertex_desc;
        }

        if (runner->r.sample_mask != ~0u)
            fatal_error("SampleMask %#x not supported.\n", runner->r.sample_mask);

        pso = [[device newRenderPipelineStateWithDescriptor:pipeline_desc error:&err] autorelease];
        ok(pso, "Failed to compile pipeline state: %s.\n", [err.description cStringUsingEncoding:NSUTF8StringEncoding]);
        [encoder setRenderPipelineState:pso];

        /* resource binding */
        argbuf = [[device newBufferWithLength:vs_argenc.encodedLength + ps_argenc.encodedLength
                options:DEFAULT_BUFFER_RESOURCE_OPTION] autorelease];
        [vs_argenc setArgumentBuffer:argbuf offset:0];
        [encoder setVertexBuffer:argbuf offset:0 atIndex:0];
        [ps_argenc setArgumentBuffer:argbuf offset:vs_argenc.encodedLength];
        [encoder setFragmentBuffer:argbuf offset:vs_argenc.encodedLength atIndex:0];

        /* we won't deal with texture/uav/sampler binding here, as the draw call fails at `compile_stage` */

        if (r->uniform_count)
        {
            cb = [[device newBufferWithBytes:r->uniforms
                    length:runner->r.uniform_count * sizeof(*runner->r.uniforms)
                    options:DEFAULT_BUFFER_RESOURCE_OPTION] autorelease];
            [vs_argenc setBuffer:cb offset:0 atIndex:0];
            [ps_argenc setBuffer:cb offset:0 atIndex:0];
            [encoder useResource:cb usage:MTLResourceUsageRead stages:MTLRenderStageVertex | MTLRenderStageFragment];
        }

        [encoder setViewport:viewport];
        [encoder setTriangleFillMode:MTLTriangleFillModeFill];
        [encoder setCullMode:MTLCullModeNone];
        [encoder setFrontFacingWinding:MTLWindingClockwise];
        [encoder setDepthBias:0 slopeScale:0 clamp:0];
        [encoder setDepthClipMode:MTLDepthClipModeClip];
        [encoder drawPrimitives:primitive_type
                vertexStart:0
                vertexCount:vertex_count
                instanceCount:instance_count];
        ret = true;
    out:
        [encoder endEncoding];

        [cmdbuf commit];
        [cmdbuf waitUntilCompleted];
    }

    for (i = 0; i < SHADER_TYPE_COUNT; ++i)
    {
        if (!runner->d3d_blobs[i])
            continue;

        vkd3d_shader_free_scan_signature_info(&runner->signatures[i]);
        ID3D10Blob_Release(runner->d3d_blobs[i]);
        runner->d3d_blobs[i] = NULL;
    }

    return ret;
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
