/*
 * Copyright 2021 Atharva Nimbalkar
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

#include "vkd3d_shader_private.h"

struct vkd3d_glsl_generator
{
    struct vsir_program *program;
    struct vkd3d_string_buffer buffer;
    struct vkd3d_shader_location location;
    struct vkd3d_shader_message_context *message_context;
    bool failed;
};

static void VKD3D_PRINTF_FUNC(3, 4) vkd3d_glsl_compiler_error(
        struct vkd3d_glsl_generator *generator,
        enum vkd3d_shader_error error, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    vkd3d_shader_verror(generator->message_context, &generator->location, error, fmt, args);
    va_end(args);
    generator->failed = true;
}

static void shader_glsl_unhandled(struct vkd3d_glsl_generator *gen, const struct vkd3d_shader_instruction *ins)
{
    vkd3d_string_buffer_printf(&gen->buffer, "/* <unhandled instruction %#x> */\n", ins->handler_idx);
    vkd3d_glsl_compiler_error(gen, VKD3D_SHADER_ERROR_GLSL_INTERNAL,
            "Internal compiler error: Unhandled instruction %#x.", ins->handler_idx);
}

static void shader_glsl_ret(struct vkd3d_glsl_generator *generator,
        const struct vkd3d_shader_instruction *ins)
{
    const struct vkd3d_shader_version *version = &generator->program->shader_version;

    /*
    * TODO: Implement in_subroutine
    * TODO: shader_glsl_generate_shader_epilogue(generator);
    */
    if (version->major >= 4)
    {
        vkd3d_string_buffer_printf(&generator->buffer, "return;\n");
    }
}

static void vkd3d_glsl_handle_instruction(struct vkd3d_glsl_generator *generator,
        const struct vkd3d_shader_instruction *instruction)
{
    generator->location = instruction->location;

    switch (instruction->handler_idx)
    {
        case VKD3DSIH_DCL_INPUT:
        case VKD3DSIH_DCL_OUTPUT:
        case VKD3DSIH_DCL_OUTPUT_SIV:
            break;
        case VKD3DSIH_RET:
            shader_glsl_ret(generator, instruction);
            break;
        default:
            shader_glsl_unhandled(generator, instruction);
            break;
    }
}

static int vkd3d_glsl_generator_generate(struct vkd3d_glsl_generator *generator, struct vkd3d_shader_code *out)
{
    const struct vkd3d_shader_instruction_array *instructions = &generator->program->instructions;
    unsigned int i;
    void *code;

    ERR("Generating a GLSL shader. This is unsupported; you get to keep all the pieces if it breaks.\n");

    vkd3d_string_buffer_printf(&generator->buffer, "#version 440\n\n");
    vkd3d_string_buffer_printf(&generator->buffer, "void main()\n{\n");

    for (i = 0; i < instructions->count; ++i)
    {
        vkd3d_glsl_handle_instruction(generator, &instructions->elements[i]);
    }

    vkd3d_string_buffer_printf(&generator->buffer, "}\n");

    if (TRACE_ON())
        vkd3d_string_buffer_trace(&generator->buffer);

    if (generator->failed)
        return VKD3D_ERROR_INVALID_SHADER;

    if ((code = vkd3d_malloc(generator->buffer.buffer_size)))
    {
        memcpy(code, generator->buffer.buffer, generator->buffer.content_size);
        out->size = generator->buffer.content_size;
        out->code = code;
    }
    else return VKD3D_ERROR_OUT_OF_MEMORY;

    return VKD3D_OK;
}

static void vkd3d_glsl_generator_cleanup(struct vkd3d_glsl_generator *gen)
{
    vkd3d_string_buffer_cleanup(&gen->buffer);
}

static void vkd3d_glsl_generator_init(struct vkd3d_glsl_generator *gen,
        struct vsir_program *program, struct vkd3d_shader_message_context *message_context)
{
    memset(gen, 0, sizeof(*gen));
    gen->program = program;
    vkd3d_string_buffer_init(&gen->buffer);
    gen->message_context = message_context;
}

int glsl_compile(struct vsir_program *program, struct vkd3d_shader_code *out,
        struct vkd3d_shader_message_context *message_context)
{
    struct vkd3d_glsl_generator generator;
    int ret;

    vkd3d_glsl_generator_init(&generator, program, message_context);
    ret = vkd3d_glsl_generator_generate(&generator, out);
    vkd3d_glsl_generator_cleanup(&generator);

    return ret;
}
