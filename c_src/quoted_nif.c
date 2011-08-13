/*
 * Copyright 2011 Magnus Klaar
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "erl_nif.h"
#include "stdbool.h"
#include "string.h"
/* The corresponding erlang functions are implemented
   in the src/quoted.erl file. */

typedef struct {
    bool is_safe_table[256];
    unsigned char unhex_table[256];
    unsigned char tohex_table[256];
} quoted_priv_data;

typedef enum {
    Q_INVALID,
    Q_LIST,
    Q_BINARY
} quoted_input_t;

static bool is_safe_tab(const unsigned char c, const quoted_priv_data* data);
static unsigned char unhex_tab(const unsigned char c, const quoted_priv_data* data);
static unsigned char tohex_tab(const unsigned char c, const quoted_priv_data* data);
static ERL_NIF_TERM unquote_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM quote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv);

static ERL_NIF_TERM true_ATOM;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    quoted_priv_data* priv = enif_alloc(sizeof(quoted_priv_data));
    int i = 0;

    enif_make_existing_atom(env, "true", &true_ATOM,  ERL_NIF_LATIN1);

    memset(priv->is_safe_table, false, 256);
    for(i = '0'; i <= '9'; i++) { priv->is_safe_table[i] = true; }
    for(i = 'a'; i <= 'z'; i++) { priv->is_safe_table[i] = true; }
    for(i = 'A'; i <= 'Z'; i++) { priv->is_safe_table[i] = true; }
    priv->is_safe_table['.'] = true;
    priv->is_safe_table['~'] = true;
    priv->is_safe_table['-'] = true;
    priv->is_safe_table['_'] = true;

    memset(priv->unhex_table, 0xF0, 256);
    for(i = '0'; i <= '9'; i++) { priv->unhex_table[i] = i - '0'; }
    for(i = 'A'; i <= 'F'; i++) { priv->unhex_table[i] = i - 'A' + 10; }
    for(i = 'a'; i <= 'f'; i++) { priv->unhex_table[i] = i - 'a' + 10; }

    memset(priv->tohex_table, false, 256);
    for(i = 0;  i <= 9;  i++) { priv->tohex_table[i] = '0' + i; }
    for(i = 10; i <= 16; i++) { priv->tohex_table[i] = 'a' + (i - 10); }

    *priv_data = priv;
    return 0;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info) {
    return 0;
}

static void unload(ErlNifEnv* env, void* priv) {
    enif_free(priv);
    return;
}

ERL_NIF_TERM unquote_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return true_ATOM;
}

ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    quoted_priv_data* priv = (quoted_priv_data*)enif_priv_data(env);
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM return_value = enif_make_badarg(env);
    quoted_input_t input_type = Q_INVALID;
    unsigned int i = 0; // Position in input
    unsigned int j = 0; // Position in output
    unsigned char c0 = 0; // Current character
    unsigned char c1 = 0; // Current character
    unsigned char c2 = 0; // Current character

    /* Determine type of input.
     * The input format also determines the output format. The caller
     * expects the output to be of the same type as the input format.
     */
    if(enif_is_list(env, argv[0])
            && enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        input_type = Q_LIST;
    }
    else if(enif_is_binary(env, argv[0])
            && enif_inspect_binary(env, argv[0], &input)) {
        input_type = Q_BINARY;
    }
    if(input_type == Q_INVALID) {
        goto out_return;
    }

    /* Scan through the input binary for any occurances of '+' or '%'.
     */
    while(i < input.size) {
        c0 = input.data[i];
        if(c0 == '%') { break; }
        if(c0 == '+') { break; }
        i++;
    }

    /* Nothing to decode. Return input term as output term */
    if(i == input.size) {
        return_value = argv[0];
        goto out_return;
    }

    /* Allocate an output buffer of the same size as the input.
     * This ensures that we only need to realloc once to shrink
     * the size of the buffer if the input buffer contains quoted
     * characters.
     *
     * XXX: A binary returned from enif_alloc_binary is not released when the
     *      NIF call returns, as binaries returned from enif_inspect..binary are.
     *      If the enif_alloc_binary call succeeds we _MUST_ release it or
     *      transfer ownership to an ERL_NIF_TERM before returning.
     */
    if(!enif_alloc_binary(input.size, &output)) {
        goto out_return;
    }
    memcpy(output.data, input.data, i);
    j = i;

    while(i < input.size) {
        c0 = input.data[i];
        if('%' == c0) {
            if(input.size < i + 3) {
                goto out_allocated;
            }
            c1 = unhex_tab(input.data[++i], priv);
            c2 = unhex_tab(input.data[++i], priv);
            if((c1 | c2) & 0xF0) {
                goto out_allocated;
            }
            c0 = (c1 << 4) | c2;
        }
        else if(c0 == '+') {
            // Spaces may be encoded as "%20" or "+". The first is standard,
            // but the second very popular. This library does " "<->"%20", 
            // but also " "<--"+" for compatibility with things like jQuery.
            c0 = ' ';
        }
        i++;
        output.data[j++] = c0;
    }

    if(input_type == Q_BINARY && enif_realloc_binary(&output, j)) {
        return_value = enif_make_binary(env, &output);
    }
    else if(input_type == Q_LIST) {
        return_value = enif_make_string_len(env, output.data, j, ERL_NIF_LATIN1);
    }
out_allocated:
    enif_release_binary(&output);
out_return:
    return return_value;
}


ERL_NIF_TERM quote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    quoted_priv_data* priv = (quoted_priv_data*)enif_priv_data(env);
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM temp;
    bool output_bin;
    

    /* Determine type of input.
     * See comment on output format in unquote_iolist(...)
     */
    if(enif_is_list(env, argv[0])) {
        output_bin = false;
        if(!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else if(enif_is_binary(env, argv[0])) {
        output_bin = true;
        if(!enif_inspect_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else {
        return enif_make_badarg(env);
    }


    /* Allocate an output buffer that is three times larger than the input
     * buffer. We only need to realloc once to shrink the size of the buffer
     * if the input contains no charactes that needs to be quoted.
     *
     * XXX: See comment in unquote_iolist.
     */
    if(!enif_alloc_binary(input.size * 3, &output)) {
        return enif_make_badarg(env);
    }

    unsigned int i = 0; // Position in input
    unsigned int j = 0; // Position in output
    unsigned char c = 0; // Current character
    while(i < input.size) {
        c = input.data[i];
        if(is_safe_tab(c, priv)) {
            output.data[j++] = c;
            i++;
        }
        else {
            output.data[j++] = '%';
            output.data[j++] = tohex_tab(c >> 4, priv);
            output.data[j++] = tohex_tab(c & 15, priv);
            i++;
        }
    }

    if(output_bin) {
        if(!enif_realloc_binary(&output, j)) {
            enif_release_binary(&output);
            return enif_make_badarg(env);
        }
        return enif_make_binary(env, &output);
    }
    else {
        temp = enif_make_string_len(env, output.data, j, ERL_NIF_LATIN1);
        enif_release_binary(&output);
        return temp;
    }
}

inline bool
is_safe_tab(const unsigned char c, const quoted_priv_data* data)
{
    return data->is_safe_table[c];
}

inline unsigned char
unhex_tab(const unsigned char c, const quoted_priv_data* data)
{
    return data->unhex_table[c];
}

inline unsigned char
tohex_tab(const unsigned char c, const quoted_priv_data* data)
{
    return data->tohex_table[c];
}

static ErlNifFunc nif_funcs[] = {
    {"is_native", 0, unquote_loaded},
    {"from_url", 1, unquote_iolist},
    {"to_url", 1, quote_iolist}
};

ERL_NIF_INIT(quoted, nif_funcs, load, reload, upgrade, unload)
