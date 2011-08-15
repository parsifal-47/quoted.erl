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
    unsigned char is_safe_table[256];
    unsigned char unhex_table[256];
    unsigned char tohex_lower_table[16];
    unsigned char tohex_upper_table[16];
} quoted_priv_data;


#define quoted_opt_lower 1
#define quoted_opt_strict 2
#define quoted_opt_plus 3
#define quoted_opts_arity 4
typedef struct {
    bool lower;
    bool strict;
    bool plus;
} quoted_opts_t;

static const quoted_opts_t quoted_opts_defaults = {
    .lower = true,
    .strict = false,
    .plus = false
};


typedef enum {
    Q_INVALID,
    Q_LIST,
    Q_BINARY
} quoted_input_t;

static bool is_safe_tab(const unsigned char c, const quoted_priv_data* data);
static unsigned char unhex_tab(const unsigned char c, const quoted_priv_data* data);
static bool read_options(ErlNifEnv* env, ERL_NIF_TERM, quoted_opts_t* opts);
static ERL_NIF_TERM unquote_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM quote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv);

static ERL_NIF_TERM true_ATOM;
static ERL_NIF_TERM false_ATOM;
static ERL_NIF_TERM options_ATOM;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    quoted_priv_data* priv = enif_alloc(sizeof(quoted_priv_data));
    if(priv == NULL) {
        return EXIT_FAILURE;
    }

    enif_make_existing_atom(env, "true", &true_ATOM,  ERL_NIF_LATIN1);
    enif_make_existing_atom(env, "false", &false_ATOM,  ERL_NIF_LATIN1);
    enif_make_existing_atom(env, "options", &options_ATOM, ERL_NIF_LATIN1);

    int i = 0;
    memset(priv->is_safe_table, 0, 256);
    for(i = '0'; i <= '9'; i++) { priv->is_safe_table[i] = 1; }
    for(i = 'a'; i <= 'z'; i++) { priv->is_safe_table[i] = 1; }
    for(i = 'A'; i <= 'Z'; i++) { priv->is_safe_table[i] = 1; }
    priv->is_safe_table['.'] = 1;
    priv->is_safe_table['~'] = 1;
    priv->is_safe_table['-'] = 1;
    priv->is_safe_table['_'] = 1;

    memset(priv->unhex_table, 0xF0, 256);
    for(i = '0'; i <= '9'; i++) { priv->unhex_table[i] = i - '0'; }
    for(i = 'A'; i <= 'F'; i++) { priv->unhex_table[i] = i - 'A' + 10; }
    for(i = 'a'; i <= 'f'; i++) { priv->unhex_table[i] = i - 'a' + 10; }

    memset(priv->tohex_lower_table, false, 16);
    for(i = 0;  i < 10;  i++) { priv->tohex_lower_table[i] = '0' + i; }
    for(i = 10; i < 16; i++) { priv->tohex_lower_table[i] = 'a' + (i - 10); }

    memset(priv->tohex_upper_table, false, 16);
    for(i = 0;  i < 10;  i++) { priv->tohex_upper_table[i] = '0' + i; }
    for(i = 10; i < 16; i++) { priv->tohex_upper_table[i] = 'A' + (i - 10); }

    *priv_data = priv;
    return EXIT_SUCCESS;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
    return EXIT_SUCCESS;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info) {
    return EXIT_SUCCESS;
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
    quoted_opts_t opts = quoted_opts_defaults;
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM return_value;
    quoted_input_t input_type = Q_INVALID;
    unsigned int i = 0; // Position in input
    unsigned int j = 0; // Position in output
    unsigned char c = 0; // Current character
    unsigned char d = 0; // Current character
    unsigned char e = 0; // Current character

    if(argc == 2 && !read_options(env, argv[1], &opts)) {
        return enif_make_badarg(env);
    }

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
        return enif_make_badarg(env);
    }

    /* Scan through the input binary for any occurances of '+' or '%'.
     */
    while(i < input.size) {
        c = input.data[i];
        if(c == '%') { break; }
        if(c == '+') { break; }
        i++;
    }

    /* Nothing to decode. Return input term as output term */
    if(i == input.size) {
        return argv[0];
    }

    /* Allocate an output buffer of the same size as the input.
     * This ensures that we only need to realloc once to shrink
     * the size of the buffer if the input buffer contains quoted
     * characters.
     */
    if(!enif_alloc_binary(input.size, &output)) {
        return enif_make_badarg(env);
    }
    memcpy(output.data, input.data, i);
    j = i;

    while(i < input.size) {
        c = input.data[i];
        if(c == '%') {
            bool size_error = input.size < i + 3;
            if(size_error && opts.strict) {
                enif_release_binary(&output);
                return enif_make_badarg(env);
            }
            else if(size_error) {
                goto output;
            }

            d = unhex_tab(input.data[++i], priv);
            e = unhex_tab(input.data[++i], priv);
            c = (d << 4) | e;

            bool hex_error = (d | e) & 0xF0;
            if(hex_error && opts.strict) {
                enif_release_binary(&output);
                return enif_make_badarg(env);
            }
            else if(hex_error) {
                i--;
                i--;
                c = '%';
            }
        }
        else if(c == '+') {
            // Spaces may be encoded as "%20" or "+". The first is standard,
            // but the second very popular. This library does " "<->"%20", 
            // but also " "<--"+" for compatibility with things like jQuery.
            c = ' ';
        }
        output:
        i++;
        output.data[j++] = c;
    }

    if(input_type == Q_BINARY && enif_realloc_binary(&output, j)) {
        return enif_make_binary(env, &output);
    }
    else if(input_type == Q_LIST) {
        return_value = enif_make_string_len(env, output.data, j, ERL_NIF_LATIN1);
        enif_release_binary(&output);
        return return_value;
    }
    enif_release_binary(&output);
    return enif_make_badarg(env);
}


ERL_NIF_TERM quote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    quoted_priv_data* priv = (quoted_priv_data*)enif_priv_data(env);
    quoted_opts_t opts = quoted_opts_defaults;
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM return_value;
    quoted_input_t input_type = Q_INVALID;
    unsigned int num_safe = 0;
    unsigned int num_unsafe = 0;
    unsigned int i = 0; // Position in input
    unsigned int j = 0; // Position in output
    unsigned char c = 0; // Current character

    if(argc == 2 && !read_options(env, argv[1], &opts)) {
        return enif_make_badarg(env);
    }
    const unsigned char* tohex_table =
        opts.lower ? priv->tohex_lower_table : priv->tohex_upper_table;
    const unsigned char* is_safe_table = priv->is_safe_table;

    /* Determine type of input.
     * See comment on output format in unquote_iolist(...)
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
        return enif_make_badarg(env);
    }

    for(i = 0; i < input.size; i++) {
        num_safe += is_safe_table[input.data[i]];
    }
    num_unsafe = input.size - num_safe;
    i = 0;

    if(num_safe == input.size) {
        return argv[0];
    }

    /* Each unsafe character will expand to at most three characters.
     * The binary should be reallocated to compensate for spaces that
     * were encoded as '+' if the plus option was enabled.
     */
    if(!enif_alloc_binary(num_safe + (num_unsafe * 3), &output)) {
        return enif_make_badarg(env);
    }

    while(i < input.size) {
        c = input.data[i];
        if(is_safe_table[c]) {
            output.data[j++] = c;
        }
        else if(c == ' ' && opts.plus) {
            output.data[j++] = '+';
        }
        else {
            output.data[j++] = '%';
            output.data[j++] = tohex_table[c >> 4];
            output.data[j++] = tohex_table[c & 0x0F];
        }
        i++;
    }

    if(input_type == Q_BINARY && enif_realloc_binary(&output, j)) {
        return enif_make_binary(env, &output);
    }
    else if(input_type == Q_LIST) {
        return_value = enif_make_string_len(env, output.data, j, ERL_NIF_LATIN1);
        enif_release_binary(&output);
        return return_value;
    }
    enif_release_binary(&output);
    return enif_make_badarg(env);
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

bool
read_options(ErlNifEnv* env, ERL_NIF_TERM rec, quoted_opts_t* opts)
{
    unsigned int arity;
    const ERL_NIF_TERM* elems;

    if(!enif_get_tuple(env, rec, &arity, &elems)) {
        return false;
    }
    if(arity != quoted_opts_arity) {
        return false;
    }
    if(!enif_is_identical(elems[0], options_ATOM)) {
        return false;
    }

    /* get #options.lower */
    if(enif_is_identical(elems[quoted_opt_lower], true_ATOM)) {
        opts->lower = true;
    }
    else if(enif_is_identical(elems[quoted_opt_lower], false_ATOM)) {
        opts->lower = false;
    }
    else{
        return false;
    }

    /* get #options.strict */
    if(enif_is_identical(elems[quoted_opt_strict], true_ATOM)) {
        opts->strict = true;
    }
    else if(enif_is_identical(elems[quoted_opt_strict], false_ATOM)) {
        opts->strict = false;
    }
    else{
        return false;
    }

    /* get #options.plus */
    if(enif_is_identical(elems[quoted_opt_plus], true_ATOM)) {
        opts->plus = true;
    }
    else if(enif_is_identical(elems[quoted_opt_plus], false_ATOM)) {
        opts->plus = false;
    }
    else{
        return false;
    }
    return true;
}


static ErlNifFunc nif_funcs[] = {
    {"is_native", 0, unquote_loaded},
    {"from_url", 1, unquote_iolist},
    {"from_url", 2, unquote_iolist},
    {"to_url", 1, quote_iolist},
    {"to_url", 2, quote_iolist}
};

ERL_NIF_INIT(quoted, nif_funcs, load, reload, upgrade, unload)
