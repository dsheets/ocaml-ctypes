(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* ML stub generation *)

type cppdec = [ `Include of string | `Define of string * string ]
type bits = Bits : _ Ctypes.typ -> bits
type macro = Tag of string | Field of string * string
type enum = One of macro list | Any of macro list

val fn : stub_name:string -> external_name:string -> Format.formatter ->
         ('a -> 'b) Ctypes.fn -> unit

val case : stub_name:string -> external_name:string -> Format.formatter ->
         ('a -> 'b) Ctypes.fn -> unit

val macro : type_name:string -> ?use_module:string -> ?default:string ->
  stub_prefix:string -> bits:bits -> Format.formatter -> enum -> unit
