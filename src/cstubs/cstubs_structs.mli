(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module type FUTURE_TYPES =
sig
  include Ctypes_types.TYP
  val lift : 'a Ctypes.typ -> 'a typ
end

module type BINDINGS = functor (F : FUTURE_TYPES) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit
