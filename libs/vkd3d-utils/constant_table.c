/*
 * D3DX9 constant table reflection
 *
 * Copyright 2010 Christian Costa
 * Copyright 2011 Travis Athougies
 * Copyright 2012 Józef Kucia
 * Copyright 2012-2013 Rico Schüller
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

#include "vkd3d_utils_private.h"
#include <vkd3d_d3dcommon.h>
#include <vkd3d_d3dx9shader.h>

#define VKD3D_SM1_FX        0x4658u
#define VKD3D_SM1_TX        0x5458u
#define VKD3D_SM1_VS_ALT    0x7ffeu
#define VKD3D_SM1_PS_ALT    0x7fffu
#define VKD3D_SM1_VS        0xfffeu
#define VKD3D_SM1_PS        0xffffu

D3DXMATRIX * WINAPI D3DXMatrixTranspose(D3DXMATRIX *out, const D3DXMATRIX *in)
{
    D3DXMATRIX m;

    TRACE("out %p, in %p.\n", out, in);

    m = *in;

    for (unsigned int i = 0; i < 4; ++i)
    {
        for (unsigned int j = 0; j < 4; ++j)
            out->u.m[i][j] = m.u.m[j][i];
    }

    return out;
}

HRESULT WINAPI D3DXFindShaderComment(const DWORD *byte_code, DWORD tag, const void **data, unsigned int *size)
{
    const DWORD *ptr = byte_code;
    uint16_t version;

    TRACE("byte_code %p, tag %#x, data %p, size %p.\n", byte_code, (uint32_t)tag, data, size);

    if (data)
        *data = NULL;
    if (size)
        *size = 0;

    if (!byte_code)
        return D3DERR_INVALIDCALL;

    version = *ptr >> 16;
    if (version != VKD3D_SM1_FX
            && version != VKD3D_SM1_TX
            && version != VKD3D_SM1_VS
            && version != VKD3D_SM1_PS
            && version != VKD3D_SM1_VS_ALT
            && version != VKD3D_SM1_PS_ALT)
    {
        WARN("Invalid version %#x, returning D3DXERR_INVALIDDATA.\n", version);
        return D3DXERR_INVALIDDATA;
    }

    while (*++ptr != D3DSIO_END)
    {
        if ((*ptr & D3DSI_OPCODE_MASK) == D3DSIO_COMMENT)
        {
            uint32_t comment_size = (*ptr & D3DSI_COMMENTSIZE_MASK) >> D3DSI_COMMENTSIZE_SHIFT;

            if (*(ptr + 1) == tag)
            {
                unsigned int ctab_size = (comment_size - 1) * sizeof(DWORD);
                const void *ctab_data = ptr + 2;

                if (size)
                    *size = ctab_size;
                if (data)
                    *data = ctab_data;
                TRACE("Returning comment data at %p with size %u.\n", ctab_data, ctab_size);
                return S_OK;
            }
            ptr += comment_size;
        }
    }

    return S_FALSE;
}

struct ctab_constant
{
    D3DXCONSTANT_DESC desc;
    struct ctab_constant *constants;
};

struct constant_table
{
    ID3DXConstantTable ID3DXConstantTable_iface;
    unsigned int refcount;
    uint8_t *ctab;
    DWORD size;
    DWORD flags;
    D3DXCONSTANTTABLE_DESC desc;
    struct ctab_constant *constants;
};

static void free_constant(struct ctab_constant *constant)
{
    if (constant->constants)
    {
        unsigned int count = constant->desc.Elements > 1 ? constant->desc.Elements : constant->desc.StructMembers;

        for (unsigned int i = 0; i < count; ++i)
            free_constant(&constant->constants[i]);
        vkd3d_free(constant->constants);
    }
}

static void free_constant_table(struct constant_table *table)
{
    if (table->constants)
    {
        for (unsigned int i = 0; i < table->desc.Constants; ++i)
            free_constant(&table->constants[i]);
        vkd3d_free(table->constants);
    }
    vkd3d_free(table->ctab);
}

static struct constant_table *impl_from_ID3DXConstantTable(ID3DXConstantTable *iface)
{
    return CONTAINING_RECORD(iface, struct constant_table, ID3DXConstantTable_iface);
}

static bool is_vertex_shader(uint32_t version)
{
    return (version >> 16) == VKD3D_SM1_VS;
}

static D3DXHANDLE handle_from_constant(struct ctab_constant *constant)
{
    return (D3DXHANDLE)constant;
}

static struct ctab_constant *get_constant_by_name(struct constant_table *table,
        struct ctab_constant *constant, const char *name);

static struct ctab_constant *get_constant_element_by_name(struct ctab_constant *constant, const char *name)
{
    unsigned int element;
    const char *part;

    TRACE("constant %p, name %s.\n", constant, debugstr_a(name));

    if (!name || !*name)
        return NULL;

    element = atoi(name);
    part = strchr(name, ']') + 1;

    if (constant->desc.Elements > element)
    {
        struct ctab_constant *c = constant->constants ? &constant->constants[element] : constant;

        switch (*part)
        {
            case '.':
                return get_constant_by_name(NULL, c, part + 1);

            case '[':
                return get_constant_element_by_name(c, part + 1);

            case '\0':
                TRACE("Returning parameter %p.\n", c);
                return c;

            default:
                FIXME("Unhandled case \"%c\".\n", *part);
                break;
        }
    }

    TRACE("Constant not found.\n");
    return NULL;
}

static struct ctab_constant *get_constant_by_name(struct constant_table *table,
        struct ctab_constant *constant, const char *name)
{
    struct ctab_constant *handles;
    unsigned int count, length;
    const char *part;

    TRACE("table %p, constant %p, name %s.\n", table, constant, debugstr_a(name));

    if (!name || !*name)
        return NULL;

    if (!constant)
    {
        count = table->desc.Constants;
        handles = table->constants;
    }
    else
    {
        count = constant->desc.StructMembers;
        handles = constant->constants;
    }

    length = strcspn(name, "[.");
    part = name + length;

    for (unsigned int i = 0; i < count; i++)
    {
        if (strlen(handles[i].desc.Name) == length && !strncmp(handles[i].desc.Name, name, length))
        {
            switch (*part++)
            {
                case '.':
                    return get_constant_by_name(NULL, &handles[i], part);

                case '[':
                    return get_constant_element_by_name(&handles[i], part);

                default:
                    TRACE("Returning parameter %p.\n", &handles[i]);
                    return &handles[i];
            }
        }
    }

    TRACE("Constant not found.\n");
    return NULL;
}

static struct ctab_constant *is_valid_sub_constant(struct ctab_constant *parent, D3DXHANDLE handle)
{
    struct ctab_constant *c;
    unsigned int count;

    /* All variables have an element count of at least 1,
     * but may not have sub-constants. */
    if (!parent->constants)
        return NULL;

    count = parent->desc.Elements > 1 ? parent->desc.Elements : parent->desc.StructMembers;
    for (unsigned int i = 0; i < count; ++i)
    {
        if (handle_from_constant(&parent->constants[i]) == handle)
            return &parent->constants[i];

        if ((c = is_valid_sub_constant(&parent->constants[i], handle)))
            return c;
    }

    return NULL;
}

static struct ctab_constant *get_valid_constant(struct constant_table *table, D3DXHANDLE handle)
{
    struct ctab_constant *c;

    if (!handle)
        return NULL;

    for (unsigned int i = 0; i < table->desc.Constants; ++i)
    {
        if (handle_from_constant(&table->constants[i]) == handle)
            return &table->constants[i];

        if ((c = is_valid_sub_constant(&table->constants[i], handle)))
            return c;
    }

    if (table->flags & D3DXCONSTTABLE_LARGEADDRESSAWARE)
        return NULL;

    return get_constant_by_name(table, NULL, handle);
}

static HRESULT WINAPI constant_table_QueryInterface(ID3DXConstantTable *iface, REFIID iid, void **out)
{
    TRACE("iface %p, iid %s, out %p.\n", iface, debugstr_guid(iid), out);

    if (IsEqualGUID(iid, &IID_IUnknown)
            || IsEqualGUID(iid, &IID_ID3DXBuffer)
            || IsEqualGUID(iid, &IID_ID3DXConstantTable))
    {
        ID3DXConstantTable_AddRef(iface);
        *out = iface;
        return S_OK;
    }

    WARN("Interface %s not found.\n", debugstr_guid(iid));

    return E_NOINTERFACE;
}

static ULONG WINAPI constant_table_AddRef(ID3DXConstantTable *iface)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    unsigned int refcount = vkd3d_atomic_increment_u32(&table->refcount);

    TRACE("%p increasing refcount to %u.\n", table, refcount);

    return refcount;
}

static ULONG WINAPI constant_table_Release(ID3DXConstantTable *iface)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    unsigned int refcount = vkd3d_atomic_decrement_u32(&table->refcount);

    TRACE("%p decreasing refcount to %u.\n", table, refcount);

    if (!refcount)
    {
        free_constant_table(table);
        vkd3d_free(table);
    }

    return refcount;
}

static void * WINAPI constant_table_GetBufferPointer(ID3DXConstantTable *iface)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p.\n", iface);

    return table->ctab;
}

static DWORD WINAPI constant_table_GetBufferSize(ID3DXConstantTable *iface)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("table %p.\n", iface);

    return table->size;
}

static HRESULT WINAPI constant_table_GetDesc(ID3DXConstantTable *iface, D3DXCONSTANTTABLE_DESC *desc)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("table %p, desc %p.\n", iface, desc);

    if (!desc)
        return D3DERR_INVALIDCALL;

    *desc = table->desc;

    return S_OK;
}

static HRESULT WINAPI constant_table_GetConstantDesc(ID3DXConstantTable *iface,
        D3DXHANDLE constant, D3DXCONSTANT_DESC *desc, unsigned int *count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    struct ctab_constant *c = get_valid_constant(table, constant);

    TRACE("table %p, constant %p, desc %p, count %p.\n", table, constant, desc, count);

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    if (desc)
        *desc = c->desc;
    if (count)
        *count = 1;

    return S_OK;
}

static unsigned int WINAPI constant_table_GetSamplerIndex(ID3DXConstantTable *iface, D3DXHANDLE constant)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    struct ctab_constant *c = get_valid_constant(table, constant);

    TRACE("table %p, constant %p.\n", table, constant);

    if (!c || c->desc.RegisterSet != D3DXRS_SAMPLER)
    {
        WARN("Invalid argument specified.\n");
        return ~0u;
    }

    TRACE("Returning RegisterIndex %u.\n", c->desc.RegisterIndex);
    return c->desc.RegisterIndex;
}

static D3DXHANDLE WINAPI constant_table_GetConstant(ID3DXConstantTable *iface, D3DXHANDLE constant, unsigned int index)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    struct ctab_constant *c;

    TRACE("table %p, constant %p, index %u.\n", table, constant, index);

    if (constant)
    {
        c = get_valid_constant(table, constant);
        if (c && index < c->desc.StructMembers)
        {
            c = &c->constants[index];
            TRACE("Returning constant %p.\n", c);
            return handle_from_constant(c);
        }
    }
    else
    {
        if (index < table->desc.Constants)
        {
            c = &table->constants[index];
            TRACE("Returning constant %p.\n", c);
            return handle_from_constant(c);
        }
    }

    WARN("Index out of range.\n");
    return NULL;
}

static D3DXHANDLE WINAPI constant_table_GetConstantByName(ID3DXConstantTable *iface,
        D3DXHANDLE constant, const char *name)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    struct ctab_constant *c = get_valid_constant(table, constant);

    TRACE("iface %p, constant %p, name %s.\n", iface, constant, debugstr_a(name));

    c = get_constant_by_name(table, c, name);
    TRACE("Returning constant %p.\n", c);

    return handle_from_constant(c);
}

static D3DXHANDLE WINAPI constant_table_GetConstantElement(
        ID3DXConstantTable *iface, D3DXHANDLE constant, unsigned int index)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    struct ctab_constant *c = get_valid_constant(table, constant);

    TRACE("table %p, constant %p, index %u.\n", table, constant, index);

    if (c && index < c->desc.Elements)
    {
        if (c->desc.Elements > 1)
            c = &c->constants[index];
        TRACE("Returning constant %p.\n", c);
        return handle_from_constant(c);
    }

    WARN("Invalid argument specified.\n");
    return NULL;
}

static uint32_t get_index(const void **in_data, unsigned int index, bool is_pointer)
{
    if (!in_data)
        return 0;

    if (is_pointer)
        return ((uint32_t **)in_data)[index / 16][index % 16];

    return (*((uint32_t **)in_data))[index];
}

static BOOL get_bool(D3DXPARAMETER_TYPE type, const void *data)
{
    switch (type)
    {
        case D3DXPT_FLOAT:
        case D3DXPT_INT:
        case D3DXPT_BOOL:
            return !!*(uint32_t *)data;

        case D3DXPT_VOID:
            return *(BOOL *)data;

        default:
            return FALSE;
    }
}

static int get_int(D3DXPARAMETER_TYPE type, const void *data)
{
    switch (type)
    {
        case D3DXPT_FLOAT:
            return (int)(*(float *)data);

        case D3DXPT_INT:
        case D3DXPT_VOID:
            return *(int *)data;

        case D3DXPT_BOOL:
            return get_bool(type, data);

        default:
            return 0;
    }
}

static float get_float(D3DXPARAMETER_TYPE type, const void *data)
{
    switch (type)
    {
        case D3DXPT_FLOAT:
        case D3DXPT_VOID:
            return *(float *)data;

        case D3DXPT_INT:
            return (float)(*(int *)data);

        case D3DXPT_BOOL:
            return (float)get_bool(type, data);

        default:
            return 0.0f;
    }
}

static void set_number(void *out_data, D3DXPARAMETER_TYPE out_type, const void *in_data, D3DXPARAMETER_TYPE in_type)
{
    if (out_type == in_type)
    {
        *(uint32_t *)out_data = *(uint32_t *)in_data;
        return;
    }

    switch (out_type)
    {
        case D3DXPT_FLOAT:
            *(float *)out_data = get_float(in_type, in_data);
            break;

        case D3DXPT_BOOL:
            *(BOOL *)out_data = get_bool(in_type, in_data);
            break;

        case D3DXPT_INT:
            *(int *)out_data = get_int(in_type, in_data);
            break;

        default:
            *(uint32_t *)out_data = 0;
            break;
    }
}

static unsigned int set(struct constant_table *table, IDirect3DDevice9 *device,
        struct ctab_constant *constant, const void **data, D3DXPARAMETER_TYPE type, unsigned int *size,
        unsigned int width, D3DXPARAMETER_CLASS class, unsigned int index, bool is_pointer)
{
    unsigned int l, regcount = 1, regsize = 1, column_count = 1, row_count = 1, ret, last = 0;
    D3DXCONSTANT_DESC *desc = &constant->desc;
    uint32_t tmp;

    if (*size < desc->Rows * desc->Columns)
    {
        *size = 0;
        return 0;
    }

    if (desc->Class == D3DXPC_STRUCT)
    {
        /*
         * Struct array sets the last complete input to the first struct element, all other
         * elements are not set.
         * E.g.: struct {int i;} s1[2];
         *       SetValue(device, "s1", [1, 2], 8) => s1 = {2, x};
         *
         *       struct {int i; int n} s2[2];
         *       SetValue(device, "s2", [1, 2, 3, 4, 5], 20) => s1 = {{3, 4}, {x, x}};
         */
        if (desc->Elements > 1)
        {
            unsigned int offset = *size / (desc->Rows * desc->Columns) - 1;

            offset = min(desc->Elements - 1, offset);
            last = offset * desc->Rows * desc->Columns;

            if ((is_pointer || class == D3DXPC_MATRIX_ROWS) && desc->RegisterSet != D3DXRS_BOOL)
            {
                set(table, device, &constant->constants[0], NULL, type, size, width, class, 0, is_pointer);
            }
            else
            {
                last += set(table, device, &constant->constants[0], data, type,
                        size, width, class, index + last, is_pointer);
            }
        }
        else
        {
            /*
             * D3DXRS_BOOL is always set. As there are only 16 bools and there are
             * exactly 16 input values, use matrix transpose.
             */
            if (class == D3DXPC_MATRIX_ROWS && desc->RegisterSet == D3DXRS_BOOL)
            {
                D3DXMATRIX mat, *m, min;

                if (is_pointer)
                    min = *(D3DXMATRIX *)(data[index / 16]);
                else
                    min = **(D3DXMATRIX **)data;

                D3DXMatrixTranspose(&mat, &min);
                m = &mat;
                for (unsigned int i = 0; i < desc->StructMembers; ++i)
                    last += set(table, device, &constant->constants[i], (const void **)&m,
                            type, size, width, D3DXPC_SCALAR, index + last, is_pointer);
            }
            /*
             * For pointers or for matrix rows, only the first member is set.
             * All other members are set to 0. table is not true for D3DXRS_BOOL.
             * E.g.: struct {int i; int n} s;
             *       SetValue(device, "s", [1, 2], 8) => s = {1, 0};
             */
            else if ((is_pointer || class == D3DXPC_MATRIX_ROWS) && desc->RegisterSet != D3DXRS_BOOL)
            {
                last = set(table, device, &constant->constants[0], data,
                        type, size, width, class, index + last, is_pointer);

                for (unsigned int i = 1; i < desc->StructMembers; ++i)
                    set(table, device, &constant->constants[i], NULL, type, size, width, class, 0, is_pointer);
            }
            else
            {
                for (unsigned int i = 0; i < desc->StructMembers; ++i)
                    last += set(table, device, &constant->constants[i], data, type,
                            size, width, D3DXPC_SCALAR, index + last, is_pointer);
            }
        }

        return last;
    }

    if (desc->Elements > 1)
    {
        for (unsigned int i = 0; i < desc->Elements && *size > 0; ++i)
        {
            last += set(table, device, &constant->constants[i], data, type, size,
                    width, class, index + last, is_pointer);

            if (class == D3DXPC_MATRIX_ROWS && desc->Class == D3DXPC_VECTOR && (i % 4) == 3)
            {
                last += 12;
                *size = *size < 12 ? 0 : *size - 12;
            }
        }

        return last;
    }

    switch (desc->Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
            regcount = min(desc->RegisterCount, desc->Rows);
            if (class == D3DXPC_MATRIX_ROWS)
                column_count = width;
            else
                row_count = width;
            regsize = desc->Columns;
            break;

        case D3DXPC_MATRIX_COLUMNS:
            regcount = min(desc->RegisterCount, desc->Columns);
            if (class == D3DXPC_MATRIX_ROWS)
                row_count = width;
            else
                column_count = width;
            regsize = desc->Rows;
            break;

        default:
            FIXME("Unhandled variable class %#x.\n", desc->Class);
            return 0;
    }

    switch (class)
    {
        case D3DXPC_SCALAR:
            ret = desc->Columns * desc->Rows;
            *size -= desc->Columns * desc->Rows;
            break;

        case D3DXPC_VECTOR:
            switch (desc->Class)
            {
                case D3DXPC_MATRIX_ROWS:
                    if (*size < regcount * 4)
                    {
                        *size = 0;
                        return 0;
                    }
                    ret = 4 * regcount;
                    *size -= 4 * regcount;
                    break;

                case D3DXPC_MATRIX_COLUMNS:
                    ret = 4 * regsize;
                    *size -= 4 * regcount;
                    break;

                case D3DXPC_SCALAR:
                    ret = 1;
                    *size -= ret;
                    break;

                case D3DXPC_VECTOR:
                    ret = 4;
                    *size -= ret;
                    break;

                default:
                    FIXME("Unhandled variable class %#x.\n", desc->Class);
                    return 0;
            }
            break;

        case D3DXPC_MATRIX_ROWS:
            switch (desc->Class)
            {
                case D3DXPC_MATRIX_ROWS:
                case D3DXPC_MATRIX_COLUMNS:
                    if (*size < 16)
                    {
                        *size = 0;
                        return 0;
                    }
                    ret = 16;
                    break;

                case D3DXPC_SCALAR:
                    ret = 4;
                    break;

                case D3DXPC_VECTOR:
                    ret = 1;
                    break;

                default:
                    FIXME("Unhandled variable class %#x.\n", desc->Class);
                    return 0;
            }
            *size -= ret;
            break;

        case D3DXPC_MATRIX_COLUMNS:
            switch (desc->Class)
            {
                case D3DXPC_MATRIX_ROWS:
                case D3DXPC_MATRIX_COLUMNS:
                    if (*size < 16)
                    {
                        *size = 0;
                        return 0;
                    }
                    ret = 16;
                    break;

                case D3DXPC_SCALAR:
                    ret = 1;
                    break;

                case D3DXPC_VECTOR:
                    ret = 4;
                    break;

                default:
                    FIXME("Unhandled variable class %#x.\n", desc->Class);
                    return 0;
            }
            *size -= ret;
            break;

        default:
            FIXME("Unhandled variable class %#x.\n", class);
            return 0;
    }

    /* Set the registers. */
    switch (desc->RegisterSet)
    {
        case D3DXRS_BOOL:
            regcount = min(desc->RegisterCount, desc->Columns * desc->Rows);
            l = 0;
            for (unsigned int i = 0; i < regcount; ++i)
            {
                uint32_t t = get_index(data, index + i / regsize * row_count + l * column_count, is_pointer);
                BOOL out;

                set_number(&tmp, desc->Type, &t, type);
                set_number(&out, D3DXPT_BOOL, &tmp, desc->Type);
                if (is_vertex_shader(table->desc.Version))
                    IDirect3DDevice9_SetVertexShaderConstantB(device, desc->RegisterIndex + i, &out, 1);
                else
                    IDirect3DDevice9_SetPixelShaderConstantB(device, desc->RegisterIndex + i, &out, 1);

                if (++l >= regsize)
                    l = 0;
            }
            return ret;

        case D3DXRS_INT4:
            for (unsigned int i = 0; i < regcount; ++i)
            {
                int32_t vec[4] = {0, 0, 1, 0};

                for (l = 0; l < regsize; ++l)
                {
                    uint32_t t = get_index(data, index + i * row_count + l * column_count, is_pointer);

                    set_number(&tmp, desc->Type, &t, type);
                    set_number(&vec[l], D3DXPT_INT, &tmp, desc->Type);
                }
                if (is_vertex_shader(table->desc.Version))
                    IDirect3DDevice9_SetVertexShaderConstantI(device, desc->RegisterIndex + i, vec, 1);
                else
                    IDirect3DDevice9_SetPixelShaderConstantI(device, desc->RegisterIndex + i, vec, 1);
            }
            return ret;

        case D3DXRS_FLOAT4:
            for (unsigned int i = 0; i < regcount; ++i)
            {
                float vec[4] = {0};

                for (l = 0; l < regsize; ++l)
                {
                    uint32_t t = get_index(data, index + i * row_count + l * column_count, is_pointer);

                    set_number(&tmp, desc->Type, &t, type);
                    set_number(&vec[l], D3DXPT_FLOAT, &tmp, desc->Type);
                }
                if (is_vertex_shader(table->desc.Version))
                    IDirect3DDevice9_SetVertexShaderConstantF(device, desc->RegisterIndex + i, vec, 1);
                else
                    IDirect3DDevice9_SetPixelShaderConstantF(device, desc->RegisterIndex + i, vec, 1);
            }
            return ret;

        default:
            FIXME("Unhandled register set %#x.\n", desc->RegisterSet);
            return 0;
    }
}

static HRESULT set_scalar(struct constant_table *table, IDirect3DDevice9 *device,
        D3DXHANDLE constant, const void *in_data, D3DXPARAMETER_TYPE in_type)
{
    struct ctab_constant *c = get_valid_constant(table, constant);
    unsigned int count = 1;

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    switch (c->desc.Class)
    {
        case D3DXPC_SCALAR:
            set(table, device, c, &in_data, in_type, &count, c->desc.Columns, D3DXPC_SCALAR, 0, false);
            return S_OK;

        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
        case D3DXPC_STRUCT:
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", c->desc.Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT set_scalar_array(struct constant_table *table, IDirect3DDevice9 *device,
        D3DXHANDLE constant, const void *in_data, unsigned int count, D3DXPARAMETER_TYPE in_type)
{
    struct ctab_constant *c = get_valid_constant(table, constant);

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    switch (c->desc.Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
        case D3DXPC_STRUCT:
            set(table, device, c, &in_data, in_type, &count, c->desc.Columns, D3DXPC_SCALAR, 0, false);
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", c->desc.Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT set_vector(struct constant_table *table, IDirect3DDevice9 *device,
        D3DXHANDLE constant, const void *in_data, D3DXPARAMETER_TYPE in_type)
{
    struct ctab_constant *c = get_valid_constant(table, constant);
    unsigned int count = 4;

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    switch (c->desc.Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_STRUCT:
            set(table, device, c, &in_data, in_type, &count, 4, D3DXPC_VECTOR, 0, false);
            return S_OK;

        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", c->desc.Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT set_vector_array(struct constant_table *table, IDirect3DDevice9 *device, D3DXHANDLE constant,
        const void *in_data, unsigned int count, D3DXPARAMETER_TYPE in_type)
{
    struct ctab_constant *c = get_valid_constant(table, constant);

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    switch (c->desc.Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
        case D3DXPC_STRUCT:
            count *= 4;
            set(table, device, c, &in_data, in_type, &count, 4, D3DXPC_VECTOR, 0, false);
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", c->desc.Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT set_matrix_array(struct constant_table *table, IDirect3DDevice9 *device,
        D3DXHANDLE constant, const void *in_data, unsigned int count, bool transpose)
{
    struct ctab_constant *c = get_valid_constant(table, constant);

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    switch (c->desc.Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
        case D3DXPC_STRUCT:
            count *= 16;
            set(table, device, c, &in_data, D3DXPT_FLOAT, &count, 4,
                    transpose ? D3DXPC_MATRIX_ROWS : D3DXPC_MATRIX_COLUMNS, 0, false);
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", c->desc.Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT set_matrix_pointer_array(struct constant_table *table, IDirect3DDevice9 *device,
        D3DXHANDLE constant, const void **in_data, unsigned int count, bool transpose)
{
    struct ctab_constant *c = get_valid_constant(table, constant);

    if (!c)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    switch (c->desc.Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
        case D3DXPC_STRUCT:
            count *= 16;
            set(table, device, c, in_data, D3DXPT_FLOAT, &count, 4,
                    transpose ? D3DXPC_MATRIX_ROWS : D3DXPC_MATRIX_COLUMNS, 0, true);
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", c->desc.Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT WINAPI constant_table_SetDefaults(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p.\n", iface, device);

    if (!device)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    for (unsigned int i = 0; i < table->desc.Constants; i++)
    {
        D3DXCONSTANT_DESC *desc = &table->constants[i].desc;
        HRESULT hr;

        if (!desc->DefaultValue)
            continue;

        switch (desc->RegisterSet)
        {
            case D3DXRS_BOOL:
                if (is_vertex_shader(table->desc.Version))
                    hr = IDirect3DDevice9_SetVertexShaderConstantB(device, desc->RegisterIndex, desc->DefaultValue,
                            desc->RegisterCount);
                else
                    hr = IDirect3DDevice9_SetPixelShaderConstantB(device, desc->RegisterIndex, desc->DefaultValue,
                            desc->RegisterCount);
                break;

            case D3DXRS_INT4:
                if (is_vertex_shader(table->desc.Version))
                    hr = IDirect3DDevice9_SetVertexShaderConstantI(device, desc->RegisterIndex, desc->DefaultValue,
                            desc->RegisterCount);
                else
                    hr = IDirect3DDevice9_SetPixelShaderConstantI(device, desc->RegisterIndex, desc->DefaultValue,
                        desc->RegisterCount);
                break;

            case D3DXRS_FLOAT4:
                if (is_vertex_shader(table->desc.Version))
                    hr = IDirect3DDevice9_SetVertexShaderConstantF(device, desc->RegisterIndex, desc->DefaultValue,
                            desc->RegisterCount);
                else
                    hr = IDirect3DDevice9_SetPixelShaderConstantF(device, desc->RegisterIndex, desc->DefaultValue,
                        desc->RegisterCount);
                break;

            default:
                FIXME("Unhandled register set %#x.\n", desc->RegisterSet);
                hr = E_NOTIMPL;
                break;
        }

        if (hr != S_OK)
            return hr;
    }

    return S_OK;
}

static HRESULT WINAPI constant_table_SetValue(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const void *data, unsigned int size)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);
    struct ctab_constant *c = get_valid_constant(table, constant);
    D3DXCONSTANT_DESC *desc;

    TRACE("iface %p, device %p, constant %p, data %p, size %u.\n", iface, device, constant, data, size);

    if (!device || !c || !data)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    desc = &c->desc;

    switch (desc->Class)
    {
        case D3DXPC_SCALAR:
        case D3DXPC_VECTOR:
        case D3DXPC_MATRIX_ROWS:
        case D3DXPC_MATRIX_COLUMNS:
        case D3DXPC_STRUCT:
            size /= 4;
            set(table, device, c, &data, desc->Type, &size, desc->Columns, D3DXPC_SCALAR, 0, false);
            return S_OK;

        default:
            FIXME("Unhandled parameter class %#x.\n", desc->Class);
            return D3DERR_INVALIDCALL;
    }
}

static HRESULT WINAPI constant_table_SetBool(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, BOOL b)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, b %d.\n", iface, device, constant, b);

    return set_scalar(table, device, constant, &b, D3DXPT_BOOL);
}

static HRESULT WINAPI constant_table_SetBoolArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const BOOL *b, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, b %p, count %d.\n", iface, device, constant, b, count);

    return set_scalar_array(table, device, constant, b, count, D3DXPT_BOOL);
}

static HRESULT WINAPI constant_table_SetInt(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, INT n)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, n %d.\n", iface, device, constant, n);

    return set_scalar(table, device, constant, &n, D3DXPT_INT);
}

static HRESULT WINAPI constant_table_SetIntArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const INT *n, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, n %p, count %d.\n", iface, device, constant, n, count);

    return set_scalar_array(table, device, constant, n, count, D3DXPT_INT);
}

static HRESULT WINAPI constant_table_SetFloat(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, float f)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, f %f.\n", iface, device, constant, f);

    return set_scalar(table, device, constant, &f, D3DXPT_FLOAT);
}

static HRESULT WINAPI constant_table_SetFloatArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const float *f, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, f %p, count %d.\n", iface, device, constant, f, count);

    return set_scalar_array(table, device, constant, f, count, D3DXPT_FLOAT);
}

static HRESULT WINAPI constant_table_SetVector(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXVECTOR4 *vector)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, vector %p.\n", iface, device, constant, vector);

    return set_vector(table, device, constant, vector, D3DXPT_FLOAT);
}

static HRESULT WINAPI constant_table_SetVectorArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXVECTOR4 *vector, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, vector %p, count %u.\n", iface, device, constant, vector, count);

    return set_vector_array(table, device, constant, vector, count, D3DXPT_FLOAT);
}

static HRESULT WINAPI constant_table_SetMatrix(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXMATRIX *matrix)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, matrix %p.\n", iface, device, constant, matrix);

    return set_matrix_array(table, device, constant, matrix, 1, false);
}

static HRESULT WINAPI constant_table_SetMatrixArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXMATRIX *matrix, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, matrix %p, count %u.\n", iface, device, constant, matrix, count);

    return set_matrix_array(table, device, constant, matrix, count, false);
}

static HRESULT WINAPI constant_table_SetMatrixPointerArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXMATRIX **matrix, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, matrix %p, count %u.\n", iface, device, constant, matrix, count);

    return set_matrix_pointer_array(table, device, constant, (const void **)matrix, count, false);
}

static HRESULT WINAPI constant_table_SetMatrixTranspose(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXMATRIX *matrix)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, matrix %p.\n", iface, device, constant, matrix);

    return set_matrix_array(table, device, constant, matrix, 1, true);
}

static HRESULT WINAPI constant_table_SetMatrixTransposeArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXMATRIX *matrix, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, matrix %p, count %u.\n", iface, device, constant, matrix, count);

    return set_matrix_array(table, device, constant, matrix, count, true);
}

static HRESULT WINAPI constant_table_SetMatrixTransposePointerArray(struct ID3DXConstantTable *iface,
        struct IDirect3DDevice9 *device, D3DXHANDLE constant, const D3DXMATRIX **matrix, unsigned int count)
{
    struct constant_table *table = impl_from_ID3DXConstantTable(iface);

    TRACE("iface %p, device %p, constant %p, matrix %p, count %u.\n", iface, device, constant, matrix, count);

    return set_matrix_pointer_array(table, device, constant, (const void **)matrix, count, true);
}

static const struct ID3DXConstantTableVtbl ID3DXConstantTable_Vtbl =
{
    /*** IUnknown methods ***/
    constant_table_QueryInterface,
    constant_table_AddRef,
    constant_table_Release,
    /*** ID3DXBuffer methods ***/
    constant_table_GetBufferPointer,
    constant_table_GetBufferSize,
    /*** ID3DXConstantTable methods ***/
    constant_table_GetDesc,
    constant_table_GetConstantDesc,
    constant_table_GetSamplerIndex,
    constant_table_GetConstant,
    constant_table_GetConstantByName,
    constant_table_GetConstantElement,
    constant_table_SetDefaults,
    constant_table_SetValue,
    constant_table_SetBool,
    constant_table_SetBoolArray,
    constant_table_SetInt,
    constant_table_SetIntArray,
    constant_table_SetFloat,
    constant_table_SetFloatArray,
    constant_table_SetVector,
    constant_table_SetVectorArray,
    constant_table_SetMatrix,
    constant_table_SetMatrixArray,
    constant_table_SetMatrixPointerArray,
    constant_table_SetMatrixTranspose,
    constant_table_SetMatrixTransposeArray,
    constant_table_SetMatrixTransposePointerArray
};

static HRESULT parse_ctab_constant_type(const uint8_t *ctab, uint32_t type_offset, struct ctab_constant *constant,
        bool is_element, uint16_t index, uint16_t max_index, uint32_t *default_value_offset, uint32_t name_offset, uint32_t regset)
{
    const D3DXSHADER_TYPEINFO *type = (const D3DXSHADER_TYPEINFO *)&ctab[type_offset];
    const D3DXSHADER_STRUCTMEMBERINFO *members = NULL;
    uint16_t size = 0;
    uint16_t count;
    HRESULT hr;

    constant->desc.DefaultValue = default_value_offset ? ctab + *default_value_offset : NULL;
    constant->desc.Class = type->Class;
    constant->desc.Type = type->Type;
    constant->desc.Rows = type->Rows;
    constant->desc.Columns = type->Columns;
    constant->desc.Elements = is_element ? 1 : type->Elements;
    constant->desc.StructMembers = type->StructMembers;
    constant->desc.Name = (char *)&ctab[name_offset];
    constant->desc.RegisterSet = regset;
    constant->desc.RegisterIndex = index;

    TRACE("name %s, elements %u, index %u, default value %p, regset %#x.\n", constant->desc.Name,
            constant->desc.Elements, index, constant->desc.DefaultValue, regset);
    TRACE("class %#x, type %#x, rows %d, columns %d, elements %d, struct members %d.\n",
            type->Class, type->Type, type->Rows, type->Columns, type->Elements, type->StructMembers);

    if (type->Elements > 1 && !is_element)
    {
        count = type->Elements;
    }
    else if ((type->Class == D3DXPC_STRUCT) && type->StructMembers)
    {
        members = (const D3DXSHADER_STRUCTMEMBERINFO *)&ctab[type->StructMemberInfo];
        count = type->StructMembers;
    }

    if (count)
    {
        if (!(constant->constants = vkd3d_calloc(count, sizeof(*constant->constants))))
            return E_OUTOFMEMORY;

        for (unsigned int i = 0; i < count; ++i)
        {
            if (FAILED(hr = parse_ctab_constant_type(ctab, members ? members[i].TypeInfo : type_offset,
                    &constant->constants[i], members == NULL, index + size, max_index, default_value_offset,
                    members ? members[i].Name : name_offset, regset)))
            {
                for (unsigned int j = 0; j < i; ++j)
                    free_constant(&constant->constants[j]);
                vkd3d_free(constant->constants);
                return hr;
            }

            size += constant->constants[i].desc.RegisterCount;
        }
    }
    else
    {
        unsigned int default_value_size = type->Columns * type->Rows * sizeof(uint32_t);

        size = type->Columns * type->Rows;

        switch (regset)
        {
            case D3DXRS_BOOL:
                break;

            case D3DXRS_FLOAT4:
            case D3DXRS_INT4:
                switch (type->Class)
                {
                    case D3DXPC_VECTOR:
                        size = 1;
                        /* fall through */
                    case D3DXPC_SCALAR:
                        default_value_size = type->Rows * 4 * sizeof(uint32_t);
                        break;

                    case D3DXPC_MATRIX_ROWS:
                        default_value_size = type->Rows * 4 * sizeof(uint32_t);
                        size = type->Rows;
                        break;

                    case D3DXPC_MATRIX_COLUMNS:
                        default_value_size = type->Columns * 4 * sizeof(uint32_t);
                        size = type->Columns;
                        break;

                    default:
                        FIXME("Unexpected class %#x.\n", type->Class);
                        break;
                }
                break;

            case D3DXRS_SAMPLER:
                size = 1;
                break;

            default:
                break;
        }

        if (default_value_offset)
            *default_value_offset += default_value_size;
    }

    constant->desc.RegisterCount = max(0, min(max_index - index, size));
    constant->desc.Bytes = 4 * constant->desc.Elements * type->Rows * type->Columns;

    return S_OK;
}

HRESULT WINAPI D3DXGetShaderConstantTableEx(const DWORD *byte_code, DWORD flags, ID3DXConstantTable **constant_table)
{
    const D3DXSHADER_CONSTANTINFO *constant_info;
    const D3DXSHADER_CONSTANTTABLE *ctab_header;
    struct constant_table *object = NULL;
    uint16_t shader_type;
    unsigned int size;
    const void *data;
    HRESULT hr;

    TRACE("byte_code %p, flags %#x, constant_table %p.\n", byte_code, (uint32_t)flags, constant_table);

    if (constant_table)
        *constant_table = NULL;

    if (!byte_code || !constant_table)
    {
        WARN("Invalid argument specified.\n");
        return D3DERR_INVALIDCALL;
    }

    shader_type = byte_code[0] >> 16;
    if (shader_type != VKD3D_SM1_PS && shader_type != VKD3D_SM1_VS)
    {
        WARN("Invalid shader type %#x.\n", shader_type);
        return S_OK;
    }

    if (flags & ~D3DXCONSTTABLE_LARGEADDRESSAWARE)
        FIXME("Ignoring flags %#x.\n", (uint32_t)flags);

    if ((hr = D3DXFindShaderComment(byte_code, TAG_CTAB, &data, &size)) != S_OK)
    {
        WARN("CTAB not found.\n");
        return D3DXERR_INVALIDDATA;
    }

    if (size < sizeof(*ctab_header))
    {
        WARN("Invalid CTAB size.\n");
        return D3DXERR_INVALIDDATA;
    }

    ctab_header = data;
    if (ctab_header->Size != sizeof(*ctab_header))
    {
        WARN("Invalid D3DXSHADER_CONSTANTTABLE size %u.\n", (uint32_t)ctab_header->Size);
        return D3DXERR_INVALIDDATA;
    }

    if (!(object = vkd3d_calloc(1, sizeof(*object))))
        return E_OUTOFMEMORY;

    object->ID3DXConstantTable_iface.lpVtbl = &ID3DXConstantTable_Vtbl;
    object->refcount = 1;

    if (!(object->ctab = vkd3d_memdup(data, size)))
    {
        vkd3d_free(object);
        return E_OUTOFMEMORY;
    }
    object->size = size;

    object->flags = flags;
    object->desc.Creator = ctab_header->Creator ? (char *)&object->ctab[ctab_header->Creator] : NULL;
    object->desc.Version = ctab_header->Version;
    object->desc.Constants = ctab_header->Constants;
    TRACE("Creator %s, Version %#x, Constants %u.\n",
            debugstr_a(object->desc.Creator), (uint32_t)object->desc.Version, object->desc.Constants);

    if (!(object->constants = vkd3d_calloc(object->desc.Constants, sizeof(*object->constants))))
    {
        hr = E_OUTOFMEMORY;
        goto error;
    }

    constant_info = (const D3DXSHADER_CONSTANTINFO *)(object->ctab + ctab_header->ConstantInfo);
    for (unsigned int i = 0; i < ctab_header->Constants; i++)
    {
        uint32_t offset = constant_info[i].DefaultValue;

        if (FAILED(hr = parse_ctab_constant_type(object->ctab, constant_info[i].TypeInfo,
                &object->constants[i], false, constant_info[i].RegisterIndex,
                constant_info[i].RegisterIndex + constant_info[i].RegisterCount,
                offset ? &offset : NULL, constant_info[i].Name, constant_info[i].RegisterSet)))
            goto error;

        if (object->constants[i].desc.RegisterSet == D3DXRS_INT4)
            object->constants[i].desc.RegisterCount = constant_info[i].RegisterCount;
    }

    *constant_table = &object->ID3DXConstantTable_iface;

    return S_OK;

error:
    free_constant_table(object);
    vkd3d_free(object);

    return hr;
}

HRESULT WINAPI D3DXGetShaderConstantTable(const DWORD *byte_code, ID3DXConstantTable **constant_table)
{
    TRACE("byte_code %p, constant_table %p.\n", byte_code, constant_table);

    return D3DXGetShaderConstantTableEx(byte_code, 0, constant_table);
}
