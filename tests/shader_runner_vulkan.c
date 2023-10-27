/*
 * Shader runner which uses libvkd3d-shader to compile HLSL -> D3D bytecode -> SPIR-V
 *
 * Copyright 2020-2022 Zebediah Figura for CodeWeavers
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

#ifndef _WIN32

#define VK_NO_PROTOTYPES
#define VKD3D_TEST_NO_DEFS
#include "config.h"
#include "vkd3d.h"
#include "vkd3d_d3dcompiler.h"
#include "shader_runner.h"
#include "vulkan_utils.h"
#include "vkd3d_test.h"

struct vulkan_resource
{
    struct resource r;

    VkBuffer buffer;
    VkBufferView buffer_view;
    VkImage image;
    VkImageView image_view;
    VkDeviceMemory memory;

    uint32_t binding;
};

static struct vulkan_resource *vulkan_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct vulkan_resource, r);
}

struct vulkan_shader_runner
{
    struct shader_runner r;
    struct shader_runner_caps caps;
    bool demote_to_helper_invocation;
    bool driver_properties;

    struct vulkan_test_context context;

    struct vkd3d_shader_scan_signature_info vs_signatures;

    struct vulkan_sampler
    {
        VkSampler vk_sampler;
        uint32_t binding;
    } samplers[MAX_SAMPLERS];
};

struct physical_device_info
{
    VkPhysicalDeviceFeatures2 features2;
    VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT interlock_features;
    VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT demote_to_helper_invocation_features;
    VkPhysicalDeviceProperties2 properties2;
    VkPhysicalDeviceDriverPropertiesKHR driver_properties;
};

static struct vulkan_shader_runner *vulkan_shader_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct vulkan_shader_runner, r);
}

static void resource_init_2d(struct vulkan_shader_runner *runner, struct vulkan_resource *resource,
        const struct resource_params *params)
{
    VkImageUsageFlagBits usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
    VkImageLayout layout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    const struct vulkan_test_context *context = &runner->context;
    VkFormat format = vkd3d_get_vk_format(params->desc.format);
    const struct resource_desc *desc = &params->desc;
    VkDevice device = context->device;
    unsigned int buffer_offset = 0;
    VkDeviceMemory staging_memory;
    VkBuffer staging_buffer;
    void *data;

    if (params->desc.type == RESOURCE_TYPE_UAV)
    {
        layout = VK_IMAGE_LAYOUT_GENERAL;
        usage |= VK_IMAGE_USAGE_STORAGE_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
    }

    resource->image = create_vulkan_2d_image(context, desc->width, desc->height, desc->level_count, desc->sample_count,
            usage, format, &resource->memory);
    resource->image_view = create_vulkan_2d_image_view(context, resource->image, format, VK_IMAGE_ASPECT_COLOR_BIT);

    if (!params->data)
    {
        begin_command_buffer(context);
        transition_image_layout(context, resource->image, VK_IMAGE_ASPECT_COLOR_BIT,
                VK_IMAGE_LAYOUT_UNDEFINED, layout);
        end_command_buffer(context);
        return;
    }

    staging_buffer = create_vulkan_buffer(context, params->data_size,
            VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &staging_memory);
    VK_CALL(vkMapMemory(device, staging_memory, 0, VK_WHOLE_SIZE, 0, &data));
    memcpy(data, params->data, params->data_size);
    VK_CALL(vkUnmapMemory(device, staging_memory));

    begin_command_buffer(context);

    transition_image_layout(context, resource->image, VK_IMAGE_ASPECT_COLOR_BIT,
            VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);

    for (unsigned int level = 0; level < params->desc.level_count; ++level)
    {
        unsigned int level_width = get_level_dimension(params->desc.width, level);
        unsigned int level_height = get_level_dimension(params->desc.height, level);
        VkBufferImageCopy region = {0};

        region.bufferOffset = buffer_offset;
        region.imageSubresource.mipLevel = level;
        region.imageSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        region.imageSubresource.layerCount = 1;
        region.imageExtent.width = level_width;
        region.imageExtent.height = level_height;
        region.imageExtent.depth = 1;
        VK_CALL(vkCmdCopyBufferToImage(context->cmd_buffer, staging_buffer, resource->image,
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region));

        buffer_offset += level_width * level_height * params->desc.texel_size;
    }

    transition_image_layout(context, resource->image, VK_IMAGE_ASPECT_COLOR_BIT, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, layout);

    end_command_buffer(context);

    VK_CALL(vkFreeMemory(device, staging_memory, NULL));
    VK_CALL(vkDestroyBuffer(device, staging_buffer, NULL));
}

static void resource_init_buffer(struct vulkan_shader_runner *runner, struct vulkan_resource *resource,
        const struct resource_params *params)
{
    const struct vulkan_test_context *context = &runner->context;
    VkFormat format = vkd3d_get_vk_format(params->desc.format);
    VkDevice device = context->device;
    VkBufferUsageFlagBits usage;
    void *data;

    if (params->desc.type == RESOURCE_TYPE_UAV)
        usage = VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT;
    else
        usage = VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT;

    /* d3d12 requires DXGI_FORMAT_UNKNOWN for structured buffers, but Vulkan requires a defined format. */
    if (format == VK_FORMAT_UNDEFINED && params->stride)
        format = VK_FORMAT_R32_UINT;

    resource->buffer = create_vulkan_buffer(context, params->data_size, usage,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &resource->memory);
    resource->buffer_view = create_vulkan_buffer_view(context, resource->buffer, format, 0);

    VK_CALL(vkMapMemory(device, resource->memory, 0, VK_WHOLE_SIZE, 0, &data));
    memcpy(data, params->data, params->data_size);
    VK_CALL(vkUnmapMemory(device, resource->memory));
}

static struct resource *vulkan_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    const struct vulkan_test_context *context = &runner->context;
    const struct resource_desc *desc = &params->desc;
    VkDevice device = context->device;
    struct vulkan_resource *resource;
    VkFormat format;
    void *data;

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    switch (params->desc.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            format = vkd3d_get_vk_format(params->desc.format);

            resource->image = create_vulkan_2d_image(context, desc->width, desc->height, desc->level_count,
                    desc->sample_count, VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                    format, &resource->memory);
            resource->image_view = create_vulkan_2d_image_view(context,
                    resource->image, format, VK_IMAGE_ASPECT_COLOR_BIT);

            begin_command_buffer(context);
            transition_image_layout(context, resource->image, VK_IMAGE_ASPECT_COLOR_BIT,
                    VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);
            end_command_buffer(context);
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            format = vkd3d_get_vk_format(params->desc.format);

            resource->image = create_vulkan_2d_image(context, desc->width, desc->height, desc->level_count,
                    desc->sample_count, VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                    format, &resource->memory);
            resource->image_view = create_vulkan_2d_image_view(context,
                    resource->image, format, VK_IMAGE_ASPECT_DEPTH_BIT);

            begin_command_buffer(context);
            transition_image_layout(context, resource->image, VK_IMAGE_ASPECT_DEPTH_BIT,
                    VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);
            end_command_buffer(context);
            break;

        case RESOURCE_TYPE_TEXTURE:
        case RESOURCE_TYPE_UAV:
            if (params->desc.dimension == RESOURCE_DIMENSION_BUFFER)
                resource_init_buffer(runner, resource, params);
            else
                resource_init_2d(runner, resource, params);
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            resource->buffer = create_vulkan_buffer(context, params->data_size,
                    VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &resource->memory);

            VK_CALL(vkMapMemory(device, resource->memory, 0, VK_WHOLE_SIZE, 0, &data));
            memcpy(data, params->data, params->data_size);
            VK_CALL(vkUnmapMemory(device, resource->memory));
            break;
    }

    return &resource->r;
}

static void vulkan_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    const struct vulkan_test_context *context = &runner->context;
    struct vulkan_resource *resource = vulkan_resource(res);
    VkDevice device = context->device;

    if (resource->memory)
        VK_CALL(vkFreeMemory(device, resource->memory, NULL));
    if (resource->image)
        VK_CALL(vkDestroyImage(device, resource->image, NULL));
    if (resource->image_view)
        VK_CALL(vkDestroyImageView(device, resource->image_view, NULL));
    if (resource->buffer)
        VK_CALL(vkDestroyBuffer(device, resource->buffer, NULL));
    if (resource->buffer_view)
        VK_CALL(vkDestroyBufferView(device, resource->buffer_view, NULL));

    free(resource);
}

static bool compile_shader(struct vulkan_shader_runner *runner,
        const char *source, const char *type, struct vkd3d_shader_code *spirv)
{
    struct vkd3d_shader_spirv_target_info spirv_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_SPIRV_TARGET_INFO};
    struct vkd3d_shader_interface_info interface_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_INTERFACE_INFO};
    struct vkd3d_shader_parameter_info parameter_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_PARAMETER_INFO};
    struct vkd3d_shader_hlsl_source_info hlsl_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO};
    struct vkd3d_shader_compile_info info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_resource_binding bindings[MAX_RESOURCES + MAX_SAMPLERS];
    struct vkd3d_shader_push_constant_buffer push_constants;
    enum vkd3d_shader_spirv_extension spirv_extensions[2];
    struct vkd3d_shader_resource_binding *binding;
    struct vkd3d_shader_compile_option options[3];
    struct vkd3d_shader_parameter1 parameters[17];
    struct vkd3d_shader_compile_option *option;
    unsigned int i, compile_options;
    struct vkd3d_shader_code dxbc;
    char profile[7];
    char *messages;
    int ret;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "2_0",
        [SHADER_MODEL_3_0] = "3_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
    };

    info.next = &hlsl_info;
    info.source.code = source;
    info.source.size = strlen(source);
    info.source_type = VKD3D_SHADER_SOURCE_HLSL;
    if (runner->r.minimum_shader_model < SHADER_MODEL_4_0)
        info.target_type = VKD3D_SHADER_TARGET_D3D_BYTECODE;
    else
        info.target_type = VKD3D_SHADER_TARGET_DXBC_TPF;

    info.log_level = VKD3D_SHADER_LOG_WARNING;

    info.options = options;
    info.option_count = 0;

    option = &options[info.option_count++];
    option->name = VKD3D_SHADER_COMPILE_OPTION_API_VERSION;
    option->value = VKD3D_SHADER_API_VERSION_1_13;

    compile_options = runner->r.compile_options;
    if (compile_options)
    {

        if (compile_options & (D3DCOMPILE_PACK_MATRIX_ROW_MAJOR | D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR))
        {
            option = &options[info.option_count++];
            option->name = VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_ORDER;
            option->value = 0;
            if (compile_options & D3DCOMPILE_PACK_MATRIX_ROW_MAJOR)
                option->value |= VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_ROW_MAJOR;
            if (compile_options & D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR)
                option->value |= VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_COLUMN_MAJOR;

            compile_options &= ~(D3DCOMPILE_PACK_MATRIX_ROW_MAJOR | D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR);
        }

        /* FIXME: ignore compatibility flag for now */
        if (compile_options & D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY)
            compile_options &= ~D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY;

        if (compile_options)
            fatal_error("Unsupported compiler options %#x.\n", compile_options);
    }

    hlsl_info.entry_point = "main";
    sprintf(profile, "%s_%s", type, shader_models[runner->r.minimum_shader_model]);
    hlsl_info.profile = profile;

    ret = vkd3d_shader_compile(&info, &dxbc, &messages);
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);
    if (ret)
        return false;

    info.next = &spirv_info;
    info.source = dxbc;
    if (runner->r.minimum_shader_model < SHADER_MODEL_4_0)
        info.source_type = VKD3D_SHADER_SOURCE_D3D_BYTECODE;
    else
        info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;
    info.target_type = VKD3D_SHADER_TARGET_SPIRV_BINARY;

    option = &options[info.option_count++];
    option->name = VKD3D_SHADER_COMPILE_OPTION_FEATURE;
    option->value = shader_runner_caps_get_feature_flags(&runner->caps);

    spirv_info.next = &interface_info;
    spirv_info.environment = VKD3D_SHADER_SPIRV_ENVIRONMENT_VULKAN_1_0;
    spirv_info.extensions = spirv_extensions;
    spirv_info.extension_count = 0;

    if (runner->caps.shader_caps[SHADER_CAP_ROV])
        spirv_extensions[spirv_info.extension_count++] = VKD3D_SHADER_SPIRV_EXTENSION_EXT_FRAGMENT_SHADER_INTERLOCK;
    if (runner->demote_to_helper_invocation)
        spirv_extensions[spirv_info.extension_count++] = VKD3D_SHADER_SPIRV_EXTENSION_EXT_DEMOTE_TO_HELPER_INVOCATION;

    push_constants.register_space = 0;
    push_constants.register_index = 0;
    push_constants.shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
    push_constants.offset = 0;
    push_constants.size = runner->r.uniform_count * sizeof(*runner->r.uniforms);

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);

        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;

            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                binding = &bindings[interface_info.binding_count++];
                if (resource->r.desc.type == RESOURCE_TYPE_UAV)
                    binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_UAV;
                else
                    binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_SRV;
                binding->register_space = 0;
                binding->register_index = resource->r.desc.slot;
                binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
                if (resource->r.desc.dimension == RESOURCE_DIMENSION_BUFFER)
                    binding->flags = VKD3D_SHADER_BINDING_FLAG_BUFFER;
                else
                    binding->flags = VKD3D_SHADER_BINDING_FLAG_IMAGE;
                binding->binding.set = 0;
                binding->binding.binding = resource->binding;
                binding->binding.count = 1;
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        binding = &bindings[interface_info.binding_count++];
        binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_SAMPLER;
        binding->register_space = 0;
        binding->register_index = runner->r.samplers[i].slot;
        binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
        binding->flags = VKD3D_SHADER_BINDING_FLAG_IMAGE;
        binding->binding.set = 0;
        binding->binding.binding = runner->samplers[i].binding;
        binding->binding.count = 1;
    }

    interface_info.next = &parameter_info;
    interface_info.bindings = bindings;

    interface_info.push_constant_buffer_count = 1;
    interface_info.push_constant_buffers = &push_constants;

    parameters[0].name = VKD3D_SHADER_PARAMETER_NAME_RASTERIZER_SAMPLE_COUNT;
    parameters[0].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[0].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_UINT32;
    parameters[0].u.immediate_constant.u.u32 = runner->r.sample_count;

    parameters[1].name = VKD3D_SHADER_PARAMETER_NAME_ALPHA_TEST_FUNC;
    parameters[1].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[1].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_UINT32;
    parameters[1].u.immediate_constant.u.u32 = runner->r.alpha_test_func;

    parameters[2].name = VKD3D_SHADER_PARAMETER_NAME_ALPHA_TEST_REF;
    parameters[2].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[2].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_FLOAT32;
    parameters[2].u.immediate_constant.u.f32 = runner->r.alpha_test_ref;

    parameters[3].name = VKD3D_SHADER_PARAMETER_NAME_FLAT_INTERPOLATION;
    parameters[3].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[3].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_UINT32;
    parameters[3].u.immediate_constant.u.u32 = runner->r.flat_shading;

    parameters[4].name = VKD3D_SHADER_PARAMETER_NAME_CLIP_PLANE_MASK;
    parameters[4].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[4].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_UINT32;
    parameters[4].u.immediate_constant.u.u32 = runner->r.clip_plane_mask;

    for (i = 0; i < 8; ++i)
    {
        parameters[5 + i].name = VKD3D_SHADER_PARAMETER_NAME_CLIP_PLANE_0 + i;
        parameters[5 + i].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
        parameters[5 + i].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_FLOAT32_VEC4;
        memcpy(parameters[5 + i].u.immediate_constant.u.f32_vec4, &runner->r.clip_planes[i], 4 * sizeof(float));
    }

    parameters[13].name = VKD3D_SHADER_PARAMETER_NAME_POINT_SIZE;
    parameters[13].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[13].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_FLOAT32;
    parameters[13].u.immediate_constant.u.f32 = runner->r.point_size;

    parameters[14].name = VKD3D_SHADER_PARAMETER_NAME_POINT_SIZE_MIN;
    parameters[14].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[14].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_FLOAT32;
    parameters[14].u.immediate_constant.u.f32 = runner->r.point_size_min;

    parameters[15].name = VKD3D_SHADER_PARAMETER_NAME_POINT_SIZE_MAX;
    parameters[15].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[15].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_FLOAT32;
    parameters[15].u.immediate_constant.u.f32 = runner->r.point_size_max;

    parameters[16].name = VKD3D_SHADER_PARAMETER_NAME_POINT_SPRITE;
    parameters[16].type = VKD3D_SHADER_PARAMETER_TYPE_IMMEDIATE_CONSTANT;
    parameters[16].data_type = VKD3D_SHADER_PARAMETER_DATA_TYPE_UINT32;
    parameters[16].u.immediate_constant.u.u32 = runner->r.point_sprite;

    parameter_info.parameter_count = ARRAY_SIZE(parameters);
    parameter_info.parameters = parameters;

    if (!strcmp(type, "vs"))
    {
        parameter_info.next = &runner->vs_signatures;

        runner->vs_signatures.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_SIGNATURE_INFO;
        runner->vs_signatures.next = NULL;
    }

    ret = vkd3d_shader_compile(&info, spirv, &messages);
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);
    vkd3d_shader_free_shader_code(&dxbc);
    if (ret)
        return false;

    return true;
}

static bool create_shader_stage(struct vulkan_shader_runner *runner,
        VkPipelineShaderStageCreateInfo *stage_info, enum shader_type type, enum VkShaderStageFlagBits stage)
{
    VkShaderModuleCreateInfo module_info = {.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO};
    const struct vulkan_test_context *context = &runner->context;
    const char *source = runner->r.shader_source[type];
    struct vkd3d_shader_code spirv;

    if (!compile_shader(runner, source, shader_type_string(type), &spirv))
        return false;

    memset(stage_info, 0, sizeof(*stage_info));
    stage_info->sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    stage_info->stage = stage;
    stage_info->pName = "main";

    module_info.codeSize = spirv.size;
    module_info.pCode = spirv.code;

    VK_CALL(vkCreateShaderModule(context->device, &module_info, NULL, &stage_info->module));
    vkd3d_shader_free_shader_code(&spirv);
    return true;
}

static VkPrimitiveTopology vulkan_primitive_topology_from_d3d(D3D_PRIMITIVE_TOPOLOGY topology)
{
    switch (topology)
    {
        default:
            fatal_error("Unhandled primitive topology %#x.\n", topology);
            /* fall through */
        case D3D_PRIMITIVE_TOPOLOGY_POINTLIST:
            return VK_PRIMITIVE_TOPOLOGY_POINT_LIST;
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST:
            return VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP:
            return VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP;
    }
}

static VkPipelineLayout create_pipeline_layout(const struct vulkan_shader_runner *runner,
        VkDescriptorSetLayout set_layout)
{
    VkPipelineLayoutCreateInfo layout_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO};
    const struct vulkan_test_context *context = &runner->context;
    VkPushConstantRange push_constant_range;
    VkPipelineLayout pipeline_layout;

    layout_desc.setLayoutCount = 1;
    layout_desc.pSetLayouts = &set_layout;

    if (runner->r.uniform_count)
    {
        layout_desc.pushConstantRangeCount = 1;
        layout_desc.pPushConstantRanges = &push_constant_range;

        push_constant_range.stageFlags = VK_SHADER_STAGE_ALL;
        push_constant_range.offset = 0;
        push_constant_range.size = runner->r.uniform_count * sizeof(*runner->r.uniforms);
    }

    VK_CALL(vkCreatePipelineLayout(context->device, &layout_desc, NULL, &pipeline_layout));

    return pipeline_layout;
}

static enum VkCompareOp vk_compare_op_from_d3d12(D3D12_COMPARISON_FUNC op)
{
    switch (op)
    {
        case D3D12_COMPARISON_FUNC_NEVER:
            return VK_COMPARE_OP_NEVER;
        case D3D12_COMPARISON_FUNC_LESS:
            return VK_COMPARE_OP_LESS;
        case D3D12_COMPARISON_FUNC_EQUAL:
            return VK_COMPARE_OP_EQUAL;
        case D3D12_COMPARISON_FUNC_LESS_EQUAL:
            return VK_COMPARE_OP_LESS_OR_EQUAL;
        case D3D12_COMPARISON_FUNC_GREATER:
            return VK_COMPARE_OP_GREATER;
        case D3D12_COMPARISON_FUNC_NOT_EQUAL:
            return VK_COMPARE_OP_NOT_EQUAL;
        case D3D12_COMPARISON_FUNC_GREATER_EQUAL:
            return VK_COMPARE_OP_GREATER_OR_EQUAL;
        case D3D12_COMPARISON_FUNC_ALWAYS:
            return VK_COMPARE_OP_ALWAYS;
        default:
            fatal_error("Unhandled compare op %#x.\n", op);
    }
}

static VkPipeline create_graphics_pipeline(struct vulkan_shader_runner *runner, VkRenderPass render_pass,
        VkPipelineLayout pipeline_layout, D3D_PRIMITIVE_TOPOLOGY primitive_topology)
{
    VkPipelineInputAssemblyStateCreateInfo ia_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO};
    VkPipelineRasterizationStateCreateInfo rs_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO};
    VkPipelineVertexInputStateCreateInfo input_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
    VkPipelineColorBlendStateCreateInfo blend_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO};
    VkPipelineMultisampleStateCreateInfo ms_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO};
    VkViewport viewport = {.y = RENDER_TARGET_HEIGHT,
            .width = RENDER_TARGET_WIDTH, .height = -RENDER_TARGET_HEIGHT, .maxDepth = 1};
    VkPipelineViewportStateCreateInfo vp_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO};
    static const VkRect2D rt_rect = {.extent.width = RENDER_TARGET_WIDTH, .extent.height = RENDER_TARGET_HEIGHT};
    VkGraphicsPipelineCreateInfo pipeline_desc = {.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO};
    VkPipelineColorBlendAttachmentState attachment_desc[MAX_RESOURCES] = {0};
    const struct vulkan_test_context *context = &runner->context;
    VkPipelineTessellationStateCreateInfo tessellation_info;
    VkVertexInputAttributeDescription input_attributes[32];
    VkPipelineDepthStencilStateCreateInfo ds_desc = {0};
    VkVertexInputBindingDescription input_bindings[32];
    VkPipelineShaderStageCreateInfo stage_desc[5];
    VkDevice device = context->device;
    unsigned int stage_count = 0;
    VkPipeline pipeline;
    unsigned int i, j;
    VkResult vr;
    int ret;

    memset(stage_desc, 0, sizeof(stage_desc));
    ret = create_shader_stage(runner, &stage_desc[stage_count++], SHADER_TYPE_VS, VK_SHADER_STAGE_VERTEX_BIT);
    ret &= create_shader_stage(runner, &stage_desc[stage_count++], SHADER_TYPE_PS, VK_SHADER_STAGE_FRAGMENT_BIT);

    if (runner->r.shader_source[SHADER_TYPE_HS])
    {
        ret &= create_shader_stage(runner, &stage_desc[stage_count++],
                SHADER_TYPE_HS, VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT);
        ret &= create_shader_stage(runner, &stage_desc[stage_count++],
                SHADER_TYPE_DS, VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT);
    }

    if (runner->r.shader_source[SHADER_TYPE_GS])
        ret &= create_shader_stage(runner, &stage_desc[stage_count++], SHADER_TYPE_GS, VK_SHADER_STAGE_GEOMETRY_BIT);

    if (!ret)
    {
        /* We ok() only when failing here, so that we don't result in a "todo
         * succeeded" when the todo applies to pipeline linking. */
        todo_if (runner->r.is_todo) ok(false, "Failed to compile shaders.\n");
        for (i = 0; i < ARRAY_SIZE(stage_desc); ++i)
            VK_CALL(vkDestroyShaderModule(device, stage_desc[i].module, NULL));
        return VK_NULL_HANDLE;
    }

    if (runner->r.input_element_count > ARRAY_SIZE(input_attributes))
        fatal_error("Input element count %zu is too high.\n", runner->r.input_element_count);

    for (i = 0; i < runner->r.input_element_count; ++i)
    {
        VkVertexInputAttributeDescription *attribute = &input_attributes[i];
        const struct input_element *element = &runner->r.input_elements[i];
        const struct vkd3d_shader_signature_element *signature_element;

        signature_element = vkd3d_shader_find_signature_element(&runner->vs_signatures.input, element->name, element->index, 0);
        ok(signature_element, "Cannot find signature element %s%u.\n", element->name, element->index);

        attribute->location = signature_element->register_index;
        attribute->binding = element->slot;
        attribute->format = vkd3d_get_vk_format(element->format);
        /* The offset will be filled below. */
    }

    input_desc.vertexAttributeDescriptionCount = runner->r.input_element_count;
    input_desc.pVertexAttributeDescriptions = input_attributes;
    input_desc.pVertexBindingDescriptions = input_bindings;

    blend_desc.attachmentCount = 0;
    blend_desc.pAttachments = attachment_desc;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);

        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
                attachment_desc[blend_desc.attachmentCount++].colorWriteMask =
                        VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT
                        | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
                break;

            case RESOURCE_TYPE_DEPTH_STENCIL:
                ds_desc.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
                ds_desc.pNext = NULL;
                ds_desc.flags = 0;
                ds_desc.depthTestEnable = VK_TRUE;
                ds_desc.depthWriteEnable = VK_TRUE;
                ds_desc.depthCompareOp = vk_compare_op_from_d3d12(runner->r.depth_func);
                ds_desc.depthBoundsTestEnable = runner->r.depth_bounds;
                ds_desc.stencilTestEnable = VK_FALSE;
                ds_desc.minDepthBounds = runner->r.depth_min;
                ds_desc.maxDepthBounds = runner->r.depth_max;
                pipeline_desc.pDepthStencilState = &ds_desc;
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
            {
                VkVertexInputBindingDescription *binding = &input_bindings[input_desc.vertexBindingDescriptionCount++];

                binding->binding = resource->r.desc.slot;
                binding->stride = 0;
                binding->inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

                for (j = 0; j < runner->r.input_element_count; ++j)
                {
                    if (runner->r.input_elements[j].slot == resource->r.desc.slot)
                    {
                        input_attributes[j].offset = binding->stride;
                        binding->stride += runner->r.input_elements[j].texel_size;
                    }
                }
                break;
            }
        }
    }

    ia_desc.topology = vulkan_primitive_topology_from_d3d(primitive_topology);

    if (runner->r.minimum_shader_model < SHADER_MODEL_4_0)
    {
        viewport.x += 0.5f;
        viewport.y += 0.5f;
    }

    vp_desc.viewportCount = 1;
    vp_desc.pViewports = &viewport;
    vp_desc.scissorCount = 1;
    vp_desc.pScissors = &rt_rect;

    rs_desc.cullMode = VK_CULL_MODE_NONE;
    rs_desc.frontFace = VK_FRONT_FACE_CLOCKWISE;
    rs_desc.lineWidth = 1.0f;

    ms_desc.rasterizationSamples = runner->r.sample_count;
    ms_desc.pSampleMask = &runner->r.sample_mask;

    pipeline_desc.stageCount = stage_count;
    pipeline_desc.pStages = stage_desc;
    pipeline_desc.pVertexInputState = &input_desc;
    pipeline_desc.pInputAssemblyState = &ia_desc;
    pipeline_desc.pViewportState = &vp_desc;
    pipeline_desc.pRasterizationState = &rs_desc;
    pipeline_desc.pMultisampleState = &ms_desc;
    pipeline_desc.pColorBlendState = &blend_desc;
    pipeline_desc.layout = pipeline_layout;
    pipeline_desc.renderPass = render_pass;
    pipeline_desc.subpass = 0;

    if (runner->r.shader_source[SHADER_TYPE_HS])
    {
        tessellation_info.sType = VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO;
        tessellation_info.pNext = NULL;
        tessellation_info.flags = 0;
        tessellation_info.patchControlPoints
                = max(primitive_topology - D3D_PRIMITIVE_TOPOLOGY_1_CONTROL_POINT_PATCHLIST + 1, 1);
        pipeline_desc.pTessellationState = &tessellation_info;
    }

    vr = VK_CALL(vkCreateGraphicsPipelines(context->device, VK_NULL_HANDLE, 1, &pipeline_desc, NULL, &pipeline));
    todo_if (runner->r.is_todo) ok(vr == VK_SUCCESS, "Failed to create graphics pipeline, vr %d.\n", vr);

    for (i = 0; i < ARRAY_SIZE(stage_desc); ++i)
        VK_CALL(vkDestroyShaderModule(device, stage_desc[i].module, NULL));
    vkd3d_shader_free_scan_signature_info(&runner->vs_signatures);

    return pipeline;
}

static VkPipeline create_compute_pipeline(struct vulkan_shader_runner *runner, VkPipelineLayout pipeline_layout)
{
    VkComputePipelineCreateInfo pipeline_desc = {.sType = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO};
    const struct vulkan_test_context *context = &runner->context;
    VkPipeline pipeline;
    bool ret;

    ret = create_shader_stage(runner, &pipeline_desc.stage, SHADER_TYPE_CS, VK_SHADER_STAGE_COMPUTE_BIT);
    todo_if (runner->r.is_todo) ok(ret, "Failed to compile shader.\n");
    if (!ret)
        return VK_NULL_HANDLE;

    pipeline_desc.layout = pipeline_layout;

    VK_CALL(vkCreateComputePipelines(context->device, VK_NULL_HANDLE, 1, &pipeline_desc, NULL, &pipeline));

    VK_CALL(vkDestroyShaderModule(context->device, pipeline_desc.stage.module, NULL));

    return pipeline;
}

static VkSamplerAddressMode vk_address_mode_from_d3d12(D3D12_TEXTURE_ADDRESS_MODE mode)
{
    switch (mode)
    {
        case D3D12_TEXTURE_ADDRESS_MODE_WRAP:
            return VK_SAMPLER_ADDRESS_MODE_REPEAT;
        case D3D12_TEXTURE_ADDRESS_MODE_MIRROR:
            return VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
        case D3D12_TEXTURE_ADDRESS_MODE_CLAMP:
            return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
        case D3D12_TEXTURE_ADDRESS_MODE_BORDER:
            return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
        default:
            fatal_error("Unhandled sampler address mode %#x.\n", mode);
            return VK_SAMPLER_ADDRESS_MODE_REPEAT;
    }
}

static VkDescriptorSetLayout create_descriptor_set_layout(struct vulkan_shader_runner *runner)
{
    VkDescriptorSetLayoutCreateInfo set_desc = {.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO};
    VkDescriptorSetLayoutBinding bindings[MAX_RESOURCES + MAX_SAMPLERS];
    const struct vulkan_test_context *context = &runner->context;
    VkDescriptorSetLayoutBinding *binding;
    VkDescriptorSetLayout set_layout;
    uint32_t binding_index = 0;
    size_t i;

    if (runner->r.resource_count > ARRAY_SIZE(bindings))
        fatal_error("Resource count %zu is too high.\n", runner->r.resource_count);

    set_desc.pBindings = bindings;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);

        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;

            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                binding = &bindings[set_desc.bindingCount++];

                resource->binding = binding_index++;

                binding->binding = resource->binding;
                if (resource->r.desc.type == RESOURCE_TYPE_UAV)
                {
                    if (resource->r.desc.dimension == RESOURCE_DIMENSION_BUFFER)
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER;
                    else
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
                }
                else
                {
                    if (resource->r.desc.dimension == RESOURCE_DIMENSION_BUFFER)
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER;
                    else
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
                }
                binding->descriptorCount = 1;
                binding->stageFlags = VK_SHADER_STAGE_ALL;
                binding->pImmutableSamplers = NULL;
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        VkSamplerCreateInfo sampler_desc = {.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO};
        struct vulkan_sampler *vulkan_sampler = &runner->samplers[i];
        const struct sampler *sampler = &runner->r.samplers[i];

        sampler_desc.magFilter = (sampler->filter & 0x4) ? VK_FILTER_LINEAR : VK_FILTER_NEAREST;
        sampler_desc.minFilter = (sampler->filter & 0x1) ? VK_FILTER_LINEAR : VK_FILTER_NEAREST;
        sampler_desc.mipmapMode = (sampler->filter & 0x10) ? VK_SAMPLER_MIPMAP_MODE_LINEAR : VK_SAMPLER_MIPMAP_MODE_NEAREST;
        sampler_desc.addressModeU = vk_address_mode_from_d3d12(sampler->u_address);
        sampler_desc.addressModeV = vk_address_mode_from_d3d12(sampler->v_address);
        sampler_desc.addressModeW = vk_address_mode_from_d3d12(sampler->w_address);
        sampler_desc.compareEnable = !!sampler->func;
        sampler_desc.compareOp = sampler->func ? vk_compare_op_from_d3d12(sampler->func) : 0;
        sampler_desc.maxLod = FLT_MAX;

        VK_CALL(vkCreateSampler(context->device, &sampler_desc, NULL, &vulkan_sampler->vk_sampler));
        vulkan_sampler->binding = binding_index++;

        binding = &bindings[set_desc.bindingCount++];

        binding->binding = vulkan_sampler->binding;
        binding->descriptorType = VK_DESCRIPTOR_TYPE_SAMPLER;
        binding->descriptorCount = 1;
        binding->stageFlags = VK_SHADER_STAGE_ALL;
        binding->pImmutableSamplers = &vulkan_sampler->vk_sampler;
    }

    VK_CALL(vkCreateDescriptorSetLayout(context->device, &set_desc, NULL, &set_layout));

    return set_layout;
}

static void bind_resources(struct vulkan_shader_runner *runner, VkPipelineBindPoint bind_point,
        VkDescriptorSetLayout set_layout, VkPipelineLayout pipeline_layout)
{
    VkDescriptorSetAllocateInfo set_desc = {.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO};
    const struct vulkan_test_context *context = &runner->context;
    VkCommandBuffer cmd_buffer = context->cmd_buffer;
    VkDescriptorSet descriptor_set;
    unsigned int i;

    set_desc.descriptorPool = context->descriptor_pool;
    set_desc.descriptorSetCount = 1;
    set_desc.pSetLayouts = &set_layout;

    VK_CALL(vkAllocateDescriptorSets(context->device, &set_desc, &descriptor_set));

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);
        VkWriteDescriptorSet write = {.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET};
        static const VkDeviceSize zero_offset;
        VkDescriptorImageInfo image_info;

        switch (resource->r.desc.type)
        {
            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                if (resource->r.desc.dimension == RESOURCE_DIMENSION_BUFFER)
                {
                    write.dstSet = descriptor_set;
                    write.dstBinding = resource->binding;
                    write.dstArrayElement = 0;
                    write.descriptorCount = 1;
                    write.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER;
                    write.pTexelBufferView = &resource->buffer_view;

                    if (resource->r.desc.type == RESOURCE_TYPE_UAV)
                        write.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER;

                    VK_CALL(vkUpdateDescriptorSets(context->device, 1, &write, 0, NULL));
                }
                else
                {
                    image_info.imageView = resource->image_view;
                    image_info.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

                    write.dstSet = descriptor_set;
                    write.dstBinding = resource->binding;
                    write.dstArrayElement = 0;
                    write.descriptorCount = 1;
                    write.descriptorType = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
                    write.pImageInfo = &image_info;

                    if (resource->r.desc.type == RESOURCE_TYPE_UAV)
                    {
                        image_info.imageLayout = VK_IMAGE_LAYOUT_GENERAL;
                        write.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
                    }

                    VK_CALL(vkUpdateDescriptorSets(context->device, 1, &write, 0, NULL));
                }
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
                if (bind_point == VK_PIPELINE_BIND_POINT_GRAPHICS)
                    VK_CALL(vkCmdBindVertexBuffers(cmd_buffer, resource->r.desc.slot, 1, &resource->buffer, &zero_offset));
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
                break;
        }
    }

    VK_CALL(vkCmdBindDescriptorSets(cmd_buffer, bind_point, pipeline_layout, 0, 1, &descriptor_set, 0, NULL));

    if (runner->r.uniform_count)
        VK_CALL(vkCmdPushConstants(cmd_buffer, pipeline_layout, VK_SHADER_STAGE_ALL, 0,
                runner->r.uniform_count * sizeof(*runner->r.uniforms), runner->r.uniforms));

    /* The descriptor set will be freed by resetting the descriptor pool. */
}

static void create_render_pass_and_framebuffer(struct vulkan_shader_runner *runner,
        VkRenderPass *render_pass, VkFramebuffer *fb)
{
    VkRenderPassCreateInfo render_pass_desc = {.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO};
    VkFramebufferCreateInfo fb_desc = {.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO};
    VkAttachmentReference ds_ref = {0}, color_refs[MAX_RESOURCES] = {0};
    VkAttachmentDescription attachment_descs[MAX_RESOURCES] = {0};
    const struct vulkan_test_context *context = &runner->context;
    unsigned int i, color_ref_count = 0, view_count = 0;
    VkSubpassDescription subpass_desc = {0};
    VkImageView views[MAX_RESOURCES];
    VkImageLayout layout;
    bool is_ds;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);
        VkAttachmentDescription *attachment_desc = &attachment_descs[view_count];
        VkAttachmentReference *color_ref = &color_refs[color_ref_count];

        if (resource->r.desc.type != RESOURCE_TYPE_RENDER_TARGET && resource->r.desc.type != RESOURCE_TYPE_DEPTH_STENCIL)
            continue;

        is_ds = resource->r.desc.type == RESOURCE_TYPE_DEPTH_STENCIL;
        layout = is_ds ? VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

        attachment_desc->format = vkd3d_get_vk_format(resource->r.desc.format);
        attachment_desc->samples = max(resource->r.desc.sample_count, 1);
        attachment_desc->loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
        attachment_desc->storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachment_desc->stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        attachment_desc->stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        attachment_desc->initialLayout = layout;
        attachment_desc->finalLayout = layout;

        if (is_ds)
        {
            ds_ref.attachment = view_count;
            ds_ref.layout = layout;
            subpass_desc.pDepthStencilAttachment = &ds_ref;
        }
        else
        {
            color_ref->attachment = view_count;
            color_ref->layout = layout;
            ++color_ref_count;
        }

        views[view_count++] = resource->image_view;
    }

    subpass_desc.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass_desc.colorAttachmentCount = color_ref_count;
    subpass_desc.pColorAttachments = color_refs;

    render_pass_desc.attachmentCount = view_count;
    render_pass_desc.pAttachments = attachment_descs;
    render_pass_desc.subpassCount = 1;
    render_pass_desc.pSubpasses = &subpass_desc;

    VK_CALL(vkCreateRenderPass(context->device, &render_pass_desc, NULL, render_pass));

    fb_desc.renderPass = *render_pass;
    fb_desc.attachmentCount = view_count;
    fb_desc.pAttachments = views;
    fb_desc.width = RENDER_TARGET_WIDTH;
    fb_desc.height = RENDER_TARGET_HEIGHT;
    fb_desc.layers = 1;

    VK_CALL(vkCreateFramebuffer(context->device, &fb_desc, NULL, fb));
}

static bool vulkan_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    const struct vulkan_test_context *context = &runner->context;
    VkCommandBuffer cmd_buffer = context->cmd_buffer;
    VkDevice device = context->device;
    VkDescriptorSetLayout set_layout;
    VkPipelineLayout pipeline_layout;
    VkPipeline pipeline;
    bool ret = false;
    unsigned int i;

    /* Create this before compiling shaders, it will assign resource bindings. */
    set_layout = create_descriptor_set_layout(runner);

    pipeline_layout = create_pipeline_layout(runner, set_layout);
    if (!(pipeline = create_compute_pipeline(runner, pipeline_layout)))
        goto out;

    begin_command_buffer(context);

    VK_CALL(vkCmdBindPipeline(cmd_buffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline));

    bind_resources(runner, VK_PIPELINE_BIND_POINT_COMPUTE, set_layout, pipeline_layout);

    VK_CALL(vkCmdDispatch(cmd_buffer, x, y, z));

    end_command_buffer(context);

    VK_CALL(vkDestroyPipeline(device, pipeline, NULL));
    VK_CALL(vkResetDescriptorPool(device, context->descriptor_pool, 0));

    ret = true;
out:
    for (i = 0; i < runner->r.sampler_count; ++i)
        VK_CALL(vkDestroySampler(device, runner->samplers[i].vk_sampler, NULL));

    VK_CALL(vkDestroyPipelineLayout(device, pipeline_layout, NULL));
    VK_CALL(vkDestroyDescriptorSetLayout(device, set_layout, NULL));

    return ret;
}

static void vulkan_runner_clear(struct shader_runner *r, struct resource *res, const struct vec4 *clear_value)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    struct vulkan_resource *resource = vulkan_resource(res);

    size_t width = resource->r.desc.width, height = resource->r.desc.height;
    const struct vulkan_test_context *context = &runner->context;
    VkSubpassDescription sub_pass_desc = {0};
    VkAttachmentDescription attachment_desc;
    VkRenderPassCreateInfo pass_desc = {0};
    VkAttachmentReference attachment_ref;
    VkDevice device = context->device;
    VkRenderPassBeginInfo begin_desc;
    VkFramebufferCreateInfo fb_desc;
    VkClearValue vk_clear_value;
    VkRenderPass render_pass;
    VkFramebuffer fb;

    attachment_desc.flags = 0;
    attachment_desc.format = vkd3d_get_vk_format(resource->r.desc.format);
    attachment_desc.samples = max(resource->r.desc.sample_count, 1);
    attachment_desc.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attachment_desc.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
    /* TODO: formats with a stencil component would a clear op here. */
    attachment_desc.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attachment_desc.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;

    sub_pass_desc.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;

    switch (resource->r.desc.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            attachment_desc.initialLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
            sub_pass_desc.colorAttachmentCount = 1;
            sub_pass_desc.pColorAttachments = &attachment_ref;
            memcpy(vk_clear_value.color.float32, clear_value, sizeof(vk_clear_value.color.float32));
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            attachment_desc.initialLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
            sub_pass_desc.pDepthStencilAttachment = &attachment_ref;
            vk_clear_value.depthStencil.depth = clear_value->x;
            vk_clear_value.depthStencil.stencil = 0;
            break;

        default:
            fatal_error("Clears are not implemented for resource type %u.\n", resource->r.desc.type);
    }

    attachment_desc.finalLayout = attachment_desc.initialLayout;

    attachment_ref.attachment = 0;
    attachment_ref.layout = attachment_desc.initialLayout;

    begin_command_buffer(context);

    pass_desc.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    pass_desc.attachmentCount = 1;
    pass_desc.pAttachments = &attachment_desc;
    pass_desc.subpassCount = 1;
    pass_desc.pSubpasses = &sub_pass_desc;
    VK_CALL(vkCreateRenderPass(device, &pass_desc, NULL, &render_pass));

    fb_desc.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    fb_desc.pNext = NULL;
    fb_desc.flags = 0;
    fb_desc.renderPass = render_pass;
    fb_desc.attachmentCount = 1;
    fb_desc.pAttachments = &resource->image_view;
    fb_desc.width = width;
    fb_desc.height = height;
    fb_desc.layers = 1;
    VK_CALL(vkCreateFramebuffer(device, &fb_desc, NULL, &fb));

    begin_desc.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    begin_desc.pNext = NULL;
    begin_desc.renderPass = render_pass;
    begin_desc.framebuffer = fb;
    begin_desc.clearValueCount = 1;
    begin_desc.pClearValues = &vk_clear_value;
    begin_desc.renderArea.offset.x = 0;
    begin_desc.renderArea.offset.y = 0;
    begin_desc.renderArea.extent.width = width;
    begin_desc.renderArea.extent.height = height;
    VK_CALL(vkCmdBeginRenderPass(context->cmd_buffer, &begin_desc, VK_SUBPASS_CONTENTS_INLINE));
    VK_CALL(vkCmdEndRenderPass(context->cmd_buffer));

    end_command_buffer(context);

    VK_CALL(vkDestroyRenderPass(device, render_pass, NULL));
    VK_CALL(vkDestroyFramebuffer(device, fb, NULL));
}

static bool vulkan_runner_draw(struct shader_runner *r,
        D3D_PRIMITIVE_TOPOLOGY primitive_topology, unsigned int vertex_count, unsigned int instance_count)
{
    VkRenderPassBeginInfo pass_begin_desc = {.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO};
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    const struct vulkan_test_context *context = &runner->context;
    VkCommandBuffer cmd_buffer = context->cmd_buffer;
    VkDevice device = context->device;
    VkDescriptorSetLayout set_layout;
    VkPipelineLayout pipeline_layout;
    VkRenderPass render_pass;
    VkPipeline pipeline;
    VkFramebuffer fb;
    bool ret = false;
    unsigned int i;

    static const VkRect2D rt_rect = {.extent.width = RENDER_TARGET_WIDTH, .extent.height = RENDER_TARGET_HEIGHT};

    create_render_pass_and_framebuffer(runner, &render_pass, &fb);

    /* Create this before compiling shaders, it will assign resource bindings. */
    set_layout = create_descriptor_set_layout(runner);

    pipeline_layout = create_pipeline_layout(runner, set_layout);
    if (!(pipeline = create_graphics_pipeline(runner, render_pass, pipeline_layout, primitive_topology)))
        goto out;

    begin_command_buffer(context);

    pass_begin_desc.renderPass = render_pass;
    pass_begin_desc.framebuffer = fb;
    pass_begin_desc.renderArea = rt_rect;

    VK_CALL(vkCmdBeginRenderPass(cmd_buffer, &pass_begin_desc, VK_SUBPASS_CONTENTS_INLINE));

    VK_CALL(vkCmdBindPipeline(cmd_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline));

    bind_resources(runner, VK_PIPELINE_BIND_POINT_GRAPHICS, set_layout, pipeline_layout);

    VK_CALL(vkCmdDraw(cmd_buffer, vertex_count, instance_count, 0, 0));

    VK_CALL(vkCmdEndRenderPass(cmd_buffer));
    end_command_buffer(context);

    VK_CALL(vkDestroyPipeline(device, pipeline, NULL));
    VK_CALL(vkResetDescriptorPool(device, context->descriptor_pool, 0));

    ret = true;
out:
    for (i = 0; i < runner->r.sampler_count; ++i)
        VK_CALL(vkDestroySampler(device, runner->samplers[i].vk_sampler, NULL));

    VK_CALL(vkDestroyPipelineLayout(device, pipeline_layout, NULL));
    VK_CALL(vkDestroyDescriptorSetLayout(device, set_layout, NULL));
    VK_CALL(vkDestroyRenderPass(device, render_pass, NULL));
    VK_CALL(vkDestroyFramebuffer(device, fb, NULL));

    return ret;
}

struct vulkan_resource_readback
{
    struct resource_readback rb;
    VkDeviceMemory memory;
    VkBuffer buffer;
};

static struct resource_readback *vulkan_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    const struct vulkan_test_context *context = &runner->context;
    struct vulkan_resource_readback *rb = malloc(sizeof(*rb));
    struct vulkan_resource *resource = vulkan_resource(res);
    VkDevice device = context->device;
    VkImageAspectFlags aspect_mask;
    VkBufferImageCopy region = {0};
    VkImageLayout layout;

    rb->rb.width = resource->r.desc.width;
    rb->rb.height = resource->r.desc.height;
    rb->rb.depth = 1;

    rb->rb.row_pitch = rb->rb.width * resource->r.desc.texel_size;

    rb->buffer = create_vulkan_buffer(context, rb->rb.row_pitch * rb->rb.height,
            VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &rb->memory);

    if (resource->r.desc.type == RESOURCE_TYPE_UAV && resource->r.desc.dimension == RESOURCE_DIMENSION_BUFFER)
    {
        void *data;

        VK_CALL(vkMapMemory(device, resource->memory, 0, VK_WHOLE_SIZE, 0, &data));
        VK_CALL(vkMapMemory(device, rb->memory, 0, VK_WHOLE_SIZE, 0, &rb->rb.data));
        memcpy(rb->rb.data, data, rb->rb.row_pitch * rb->rb.height);
        VK_CALL(vkUnmapMemory(device, resource->memory));
    }
    else
    {
        struct resource_desc resolved_desc = resource->r.desc;
        VkDeviceMemory resolved_memory;
        VkImage resolved_image;

        aspect_mask = (resource->r.desc.type == RESOURCE_TYPE_DEPTH_STENCIL)
                ? VK_IMAGE_ASPECT_DEPTH_BIT : VK_IMAGE_ASPECT_COLOR_BIT;

        if (resource->r.desc.type == RESOURCE_TYPE_RENDER_TARGET)
            layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        else if (resource->r.desc.type == RESOURCE_TYPE_DEPTH_STENCIL)
            layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        else
            layout = VK_IMAGE_LAYOUT_GENERAL;

        begin_command_buffer(context);

        transition_image_layout(context, resource->image, aspect_mask, layout, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);

        region.imageSubresource.aspectMask = aspect_mask;
        region.imageSubresource.layerCount = 1;
        region.imageExtent.width = resource->r.desc.width;
        region.imageExtent.height = resource->r.desc.height;
        region.imageExtent.depth = 1;

        if (resource->r.desc.sample_count > 1)
        {
            VkImageResolve resolve_region = {{0}};

            resolve_region.srcSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
            resolve_region.srcSubresource.layerCount = 1;
            resolve_region.dstSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
            resolve_region.dstSubresource.layerCount = 1;
            resolve_region.extent.width = resource->r.desc.width;
            resolve_region.extent.height = resource->r.desc.height;
            resolve_region.extent.depth = 1;

            resolved_desc.sample_count = 1;
            resolved_image = create_vulkan_2d_image(context, resolved_desc.width, resolved_desc.height,
                    resolved_desc.level_count, resolved_desc.sample_count,
                    VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT,
                    vkd3d_get_vk_format(resource->r.desc.format), &resolved_memory);
            transition_image_layout(context, resolved_image, VK_IMAGE_ASPECT_COLOR_BIT,
                    VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);

            VK_CALL(vkCmdResolveImage(context->cmd_buffer, resource->image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    resolved_image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &resolve_region));
            transition_image_layout(context, resolved_image, VK_IMAGE_ASPECT_COLOR_BIT,
                    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
            VK_CALL(vkCmdCopyImageToBuffer(context->cmd_buffer, resolved_image,
                    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, rb->buffer, 1, &region));
        }
        else
        {
            VK_CALL(vkCmdCopyImageToBuffer(context->cmd_buffer, resource->image,
                    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, rb->buffer, 1, &region));
        }

        transition_image_layout(context, resource->image, aspect_mask, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, layout);

        end_command_buffer(context);

        if (resource->r.desc.sample_count > 1)
        {
            VK_CALL(vkFreeMemory(device, resolved_memory, NULL));
            VK_CALL(vkDestroyImage(device, resolved_image, NULL));
        }

        VK_CALL(vkMapMemory(device, rb->memory, 0, VK_WHOLE_SIZE, 0, &rb->rb.data));
    }

    return &rb->rb;
}

static void vulkan_runner_release_readback(struct shader_runner *r, struct resource_readback *rb)
{
    struct vulkan_resource_readback *vulkan_rb = CONTAINING_RECORD(rb, struct vulkan_resource_readback, rb);
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    const struct vulkan_test_context *context = &runner->context;
    VkDevice device = context->device;

    VK_CALL(vkUnmapMemory(device, vulkan_rb->memory));

    VK_CALL(vkFreeMemory(device, vulkan_rb->memory, NULL));
    VK_CALL(vkDestroyBuffer(device, vulkan_rb->buffer, NULL));
    free(vulkan_rb);
}

static const struct shader_runner_ops vulkan_runner_ops =
{
    .create_resource = vulkan_runner_create_resource,
    .destroy_resource = vulkan_runner_destroy_resource,
    .dispatch = vulkan_runner_dispatch,
    .clear = vulkan_runner_clear,
    .draw = vulkan_runner_draw,
    .get_resource_readback = vulkan_runner_get_resource_readback,
    .release_readback = vulkan_runner_release_readback,
};

static bool check_device_extensions(struct vulkan_shader_runner *runner,
        struct vulkan_extension_list *enabled_extensions)
{
    const struct vulkan_test_context *context = &runner->context;
    VkPhysicalDevice phys_device = context->phys_device;
    VkExtensionProperties *extensions;
    uint32_t i, count;

    static const struct
    {
        const char *name;
        bool required;
    }
    device_extensions[] =
    {
        {VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME},
        {VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME},
        {VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME, true},
        {VK_KHR_MAINTENANCE1_EXTENSION_NAME, true},
        {VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME},
    };

    enabled_extensions->names = calloc(ARRAY_SIZE(device_extensions), sizeof(*enabled_extensions->names));
    enabled_extensions->count = 0;

    VK_CALL(vkEnumerateDeviceExtensionProperties(phys_device, NULL, &count, NULL));
    extensions = calloc(count, sizeof(*extensions));
    VK_CALL(vkEnumerateDeviceExtensionProperties(phys_device, NULL, &count, extensions));

    for (i = 0; i < ARRAY_SIZE(device_extensions); ++i)
    {
        const char *name = device_extensions[i].name;

        if (vk_extension_properties_contain(extensions, count, name))
        {
            enabled_extensions->names[enabled_extensions->count++] = name;
            if (!strcmp(name, VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME))
                runner->caps.shader_caps[SHADER_CAP_ROV] = true;
            if (!strcmp(name, VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME))
                runner->demote_to_helper_invocation = true;
            if (!strcmp(name, VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME))
                runner->driver_properties = true;
            continue;
        }

        if (device_extensions[i].required)
        {
            skip("The selected Vulkan device does not support %s.\n", name);
            free(enabled_extensions->names);
            free(extensions);
            return false;
        }
    }

    free(extensions);
    return true;
}

static void get_physical_device_info(struct vulkan_shader_runner *runner, struct physical_device_info *info)
{
    const struct vulkan_test_context *context = &runner->context;
    memset(info, 0, sizeof(*info));

    info->features2.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2;
    if (runner->caps.shader_caps[SHADER_CAP_ROV])
    {
        info->features2.pNext = &info->interlock_features;
        info->interlock_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT;
    }

    if (runner->demote_to_helper_invocation)
    {
        void *list = info->features2.pNext;

        info->features2.pNext = &info->demote_to_helper_invocation_features;
        info->demote_to_helper_invocation_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT;
        info->demote_to_helper_invocation_features.pNext = list;
    }

    if (context->vkGetPhysicalDeviceFeatures2KHR)
        VK_CALL(vkGetPhysicalDeviceFeatures2KHR(context->phys_device, &info->features2));
    else
        VK_CALL(vkGetPhysicalDeviceFeatures(context->phys_device, &info->features2.features));

    if (runner->driver_properties)
    {
        void *list = info->properties2.pNext;

        info->properties2.pNext = &info->driver_properties;
        info->driver_properties.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR;
        info->driver_properties.pNext = list;
    }

    if (context->vkGetPhysicalDeviceFeatures2KHR)
        VK_CALL(vkGetPhysicalDeviceProperties2KHR(context->phys_device, &info->properties2));
    else
        VK_CALL(vkGetPhysicalDeviceProperties(context->phys_device, &info->properties2.properties));
}

static uint32_t get_format_support(const struct vulkan_test_context *context, enum DXGI_FORMAT format)
{
    VkFormatProperties properties;
    uint32_t ret = 0;

    VK_CALL(vkGetPhysicalDeviceFormatProperties(context->phys_device, vkd3d_get_vk_format(format), &properties));
    if ((properties.linearTilingFeatures | properties.optimalTilingFeatures) & VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT)
        ret |= FORMAT_CAP_UAV_LOAD;

    return ret;
}

static bool init_vulkan_runner(struct vulkan_shader_runner *runner)
{
    VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT demote_to_helper_invocation_features;
    VkDeviceQueueCreateInfo queue_desc = {.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO};
    VkDeviceCreateInfo device_desc = {.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO};
    VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT interlock_features;
    struct vulkan_test_context *context = &runner->context;
    struct vulkan_extension_list enabled_extensions;
    const VkPhysicalDeviceFeatures *ret_features;
    static const float queue_priority = 1.0f;
    struct physical_device_info device_info;
    VkPhysicalDeviceFeatures features;
    VkFormatProperties format_props;
    uint32_t graphics_index;
    bool b;

    static const char *instance_extensions[] =
    {
        VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME,
    };
    static const enum DXGI_FORMAT formats[] =
    {
        DXGI_FORMAT_R32_FLOAT,
        DXGI_FORMAT_R32_UINT,
        DXGI_FORMAT_R32_SINT,
        DXGI_FORMAT_R32G32B32A32_FLOAT,
        DXGI_FORMAT_R32G32B32A32_UINT,
        DXGI_FORMAT_R32G32B32A32_SINT,
        DXGI_FORMAT_R16G16B16A16_FLOAT,
        DXGI_FORMAT_R16G16B16A16_UINT,
        DXGI_FORMAT_R16G16B16A16_SINT,
        DXGI_FORMAT_R8G8B8A8_UNORM,
        DXGI_FORMAT_R8G8B8A8_UINT,
        DXGI_FORMAT_R8G8B8A8_SINT,
        DXGI_FORMAT_R16_FLOAT,
        DXGI_FORMAT_R16_UINT,
        DXGI_FORMAT_R16_SINT,
        DXGI_FORMAT_R8_UNORM,
        DXGI_FORMAT_R8_UINT,
        DXGI_FORMAT_R8_SINT,
    };
    static const char *const tags[] =
    {
        "vulkan",
    };

    static const char *const mvk_tags[] =
    {
        "vulkan",
        "mvk",
    };

    if (!vulkan_test_context_init_instance(context, instance_extensions, ARRAY_SIZE(instance_extensions)))
        return false;

    if (!get_vulkan_queue_index(context, VK_QUEUE_GRAPHICS_BIT, &graphics_index))
    {
        skip("The selected Vulkan device does not support graphics operations.\n");
        goto out_destroy_context;
    }

    device_desc.pQueueCreateInfos = &queue_desc;
    device_desc.queueCreateInfoCount = 1;

    queue_desc.queueFamilyIndex = graphics_index;
    queue_desc.queueCount = 1;
    queue_desc.pQueuePriorities = &queue_priority;

    if (!check_device_extensions(runner, &enabled_extensions))
        goto out_destroy_context;
    device_desc.ppEnabledExtensionNames = enabled_extensions.names;
    device_desc.enabledExtensionCount = enabled_extensions.count;

    VK_CALL(vkGetPhysicalDeviceFormatProperties(context->phys_device, VK_FORMAT_R32G32B32A32_SFLOAT, &format_props));
    if (!(format_props.optimalTilingFeatures & VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT))
    {
        skip("The selected Vulkan device does not support R32G32B32A32_SFLOAT render targets.\n");
        goto out_destroy_context;
    }

    runner->caps.runner = "Vulkan";
    get_physical_device_info(runner, &device_info);
    ret_features = &device_info.features2.features;

    if (device_info.driver_properties.driverID == VK_DRIVER_ID_MOLTENVK)
    {
        runner->caps.tags = mvk_tags;
        runner->caps.tag_count = ARRAY_SIZE(mvk_tags);
    }
    else
    {
        runner->caps.tags = tags;
        runner->caps.tag_count = ARRAY_SIZE(tags);
    }

    runner->caps.shader_caps[SHADER_CAP_CLIP_PLANES] = true;
    runner->caps.shader_caps[SHADER_CAP_POINT_SIZE] = true;

    device_desc.pEnabledFeatures = &features;
    memset(&features, 0, sizeof(features));

    /* FIXME: Probably make these optional. */

#define ENABLE_FEATURE(x) \
    if (!ret_features->x) \
    { \
        skip("The selected Vulkan device does not support " #x ".\n"); \
        goto out_destroy_context; \
    } \
    features.x = VK_TRUE

    ENABLE_FEATURE(fragmentStoresAndAtomics);
    ENABLE_FEATURE(shaderClipDistance);
    ENABLE_FEATURE(shaderImageGatherExtended);
    ENABLE_FEATURE(shaderStorageImageWriteWithoutFormat);

    if (ret_features->geometryShader)
    {
        features.geometryShader = VK_TRUE;
        runner->caps.shader_caps[SHADER_CAP_GEOMETRY_SHADER] = true;
    }

    if (ret_features->shaderFloat64)
    {
        features.shaderFloat64 = VK_TRUE;
        runner->caps.shader_caps[SHADER_CAP_FLOAT64] = true;
    }

    if (ret_features->shaderInt64)
    {
        features.shaderInt64 = VK_TRUE;
        runner->caps.shader_caps[SHADER_CAP_INT64] = true;
    }

    if (ret_features->depthBounds)
    {
        features.depthBounds = VK_TRUE;
        runner->caps.shader_caps[SHADER_CAP_DEPTH_BOUNDS] = true;
    }

    if (device_info.interlock_features.fragmentShaderSampleInterlock
            && device_info.interlock_features.fragmentShaderPixelInterlock)
    {
        memset(&interlock_features, 0, sizeof(interlock_features));
        interlock_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT;
        interlock_features.pNext = (void *)device_desc.pNext;
        interlock_features.fragmentShaderSampleInterlock = VK_TRUE;
        interlock_features.fragmentShaderPixelInterlock = VK_TRUE;
        device_desc.pNext = &interlock_features;
    }
    else
    {
        runner->caps.shader_caps[SHADER_CAP_ROV] = false;
    }

    if (device_info.demote_to_helper_invocation_features.shaderDemoteToHelperInvocation)
    {
        memset(&demote_to_helper_invocation_features, 0, sizeof(demote_to_helper_invocation_features));
        demote_to_helper_invocation_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT;
        demote_to_helper_invocation_features.pNext = (void *)device_desc.pNext;
        demote_to_helper_invocation_features.shaderDemoteToHelperInvocation = VK_TRUE;
        device_desc.pNext = &demote_to_helper_invocation_features;
    }
    else
    {
        runner->demote_to_helper_invocation = false;
    }

    if (device_info.features2.features.shaderStorageImageReadWithoutFormat)
        runner->caps.format_caps[DXGI_FORMAT_UNKNOWN] |= FORMAT_CAP_UAV_LOAD;
    for (unsigned int i = 0; i < ARRAY_SIZE(formats); ++i)
    {
        runner->caps.format_caps[formats[i]] = get_format_support(context, formats[i]);
    }

    b = vulkan_test_context_init_device(context, &device_desc, graphics_index, MAX_RESOURCES, MAX_SAMPLERS);
    free(enabled_extensions.names);

    if (b)
        return true;

out_destroy_context:
    vulkan_test_context_destroy(context);
    return false;
};

void run_shader_tests_vulkan(void)
{
    struct vulkan_shader_runner runner = {0};

    if (!init_vulkan_runner(&runner))
        return;

    runner.caps.minimum_shader_model = SHADER_MODEL_2_0;
    runner.caps.maximum_shader_model = SHADER_MODEL_3_0;
    run_shader_tests(&runner.r, &runner.caps, &vulkan_runner_ops, NULL);

    runner.caps.minimum_shader_model = SHADER_MODEL_4_0;
    runner.caps.maximum_shader_model = SHADER_MODEL_5_1;
    run_shader_tests(&runner.r, &runner.caps, &vulkan_runner_ops, NULL);

    vulkan_test_context_destroy(&runner.context);
}

#endif
