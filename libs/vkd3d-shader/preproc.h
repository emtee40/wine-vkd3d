/*
 * HLSL preprocessor
 *
 * Copyright 2020 Zebediah Figura for CodeWeavers
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

#ifndef __VKD3D_SHADER_PREPROC_H
#define __VKD3D_SHADER_PREPROC_H

#include "vkd3d_shader_private.h"
#include "rbtree.h"

struct preproc_if_state
{
    /* Are we currently in a "true" block? */
    bool current_true;
    /* Have we seen a "true" block in this #if..#endif yet? */
    bool seen_true;
    /* Have we seen an #else yet? */
    bool seen_else;
};

struct preproc_buffer
{
    void *lexer_buffer;
    struct vkd3d_shader_location location;
};

struct preproc_file
{
    struct preproc_buffer buffer;
    struct vkd3d_shader_code code;
    char *filename;

    struct preproc_if_state *if_stack;
    size_t if_count, if_stack_size;
};

struct preproc_text
{
    struct vkd3d_string_buffer text;
    struct vkd3d_shader_location location;
};

struct preproc_expansion
{
    struct preproc_buffer buffer;
    const struct preproc_text *text;
};

struct preproc_macro
{
    struct rb_entry entry;
    char *name;

    struct preproc_text body;
};

struct preproc_ctx
{
    const struct vkd3d_shader_preprocess_info *preprocess_info;
    void *scanner;

    struct vkd3d_shader_message_context *message_context;
    struct vkd3d_string_buffer buffer;

    struct preproc_file *file_stack;
    size_t file_count, file_stack_size;

    struct preproc_expansion *expansion_stack;
    size_t expansion_count, expansion_stack_size;

    struct rb_tree macros;

    int current_directive;

    bool last_was_newline;
    bool last_was_eof;

    bool error;
};

void preproc_close_include(struct preproc_ctx *ctx, const struct vkd3d_shader_code *code) DECLSPEC_HIDDEN;
struct preproc_macro *preproc_find_macro(struct preproc_ctx *ctx, const char *name) DECLSPEC_HIDDEN;
void preproc_free_macro(struct preproc_macro *macro) DECLSPEC_HIDDEN;
bool preproc_push_include(struct preproc_ctx *ctx, char *filename, const struct vkd3d_shader_code *code) DECLSPEC_HIDDEN;
void preproc_warning(struct preproc_ctx *ctx, const struct vkd3d_shader_location *loc,
        enum vkd3d_shader_error error, const char *format, ...) VKD3D_PRINTF_FUNC(4, 5) DECLSPEC_HIDDEN;

static inline struct preproc_file *preproc_get_top_file(struct preproc_ctx *ctx)
{
    assert(ctx->file_count);
    return &ctx->file_stack[ctx->file_count - 1];
}

#endif
