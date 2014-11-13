/*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CSTUBS_INTERNALS_H
#define CSTUBS_INTERNALS_H

/* Types and functions used by generated C code. */

#include "ctypes/primitives.h"
#include "ctypes/complex_stubs.h"
#include "ctypes/raw_pointer.h"
#include "ctypes/managed_buffer_stubs.h"
#define CTYPES_PTR_OF_OCAML_STRING(s) \
  (String_val(Field(s, 1)) + Int_val(Field(s, 0)))

/* #define CTYPES_FUNCALL(F, BUF, ...)  \ */
/*   (*((struct F ## _frame *)BUF) =    \ */
/*    (struct F ## _frame){             \ */
/*     F ## _name, __VA_ARGS__          \ */
/*    },                                \ */
/*    cstubs_release_lock(BUF),         \ */
/*    ((struct F ## _frame *)BUF)->return_value)  */
#ifndef CTYPES_FUNCALL
#define CTYPES_FUNCALL(F, ...)  ((F)(__VA_ARGS__))
#endif

#endif /* CSTUBS_INTERNALS_H */
