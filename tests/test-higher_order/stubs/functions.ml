(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the higher order tests. *)

open Ctypes
open Foreign

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F
  let higher_order_1 = foreign "higher_order_1"
    (funptr Ctypes.(int @-> int @-> returning int) @-> int @-> int @-> returning int)

  let higher_order_3 = foreign "higher_order_3"
    (funptr Ctypes.(funptr Ctypes.(int @-> int @-> returning int) @->
             int @-> int @-> returning int) @->
     funptr Ctypes.(int @-> int @-> returning int) @->
     int @-> int @-> returning int)

  let returning_funptr = foreign "returning_funptr"
    (int @-> returning (funptr Ctypes.(int @-> int @-> returning int)))

  let callback_returns_funptr = foreign "callback_returns_funptr"
    (funptr Ctypes.(int @-> returning (funptr Ctypes.(int @-> returning int))) @->
     int @-> returning int)

  let register_callback = foreign "register_callback"
      (funptr Ctypes.(void @-> returning int) @-> returning void)

  let call_registered_callback = foreign "call_registered_callback"
      (int @-> int @-> returning void)
end
