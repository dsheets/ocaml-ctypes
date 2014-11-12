(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Operations for generating C bindings stubs. *)

module type FOREIGN =
sig
  type 'a fn (* The result type *)

  type 'a f   (* The abstract function type *)
  type 'a comp (* The computation type *)

  val returning : 'a Ctypes.typ -> 'a comp f
  val (@->) : 'a Ctypes.typ -> 'b f -> ('a -> 'b) f

  val foreign : string -> ('a -> 'b) f -> ('a -> 'b) fn
end

module type BINDINGS = functor (F : FOREIGN with type 'a fn = unit) -> sig end

val write_c : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** [write_c fmt ~prefix bindings] generates C stubs for the functions bound
    with [foreign] in [bindings].  The stubs are intended to be used in
    conjunction with the ML code generated by {!write_ml}.

    The generated code uses definitions exposed in the header file
    [cstubs_internals.h].
*)

val write_ml : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** [write_ml fmt ~prefix bindings] generates ML bindings for the functions
    bound with [foreign] in [bindings].  The generated code conforms to the
    {!FOREIGN} interface.

    The generated code uses definitions exposed in the module
    [Cstubs_internals]. *)

val write_enum : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** Write a C enum definition with one enumeration constant for each foreign
    binding and one enumeration constant giving the number of entries. *)

val write_frame_structs : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** Write a C struct definition to hold the function identifier, arguments and
    return type for each foreign binding and a variable giving the struct of
    the largest struct. *)

val write_remote_dispatcher : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** Write a definition of a C function that reads a call frame struct from a
    buffer and dispatches to the appropriate function. *)

val write_remote_initializer_ml : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** Write some module initializer code that initializes the shared memory and
    starts the remote process. *)

val write_remote_initializer_c : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
(** Write an initializer function that initializes the shared memory and
    starts the remote process. *)
