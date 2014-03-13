(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs public interface. *)

module type FOREIGN =
sig
  type 'a fn
  val foreign : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn
  val funptr : ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn Ctypes.typ

  open Ctypes
  module Enum : sig
    type 'a constr
    type 'a t
    type from = Cstubs_generate_c.cppdec

    val macro : from:from list -> type_name:string -> ?use_module:string ->
      ?default:string -> 'i typ -> 'a t -> 'a typ
    val one : 'a constr list -> 'a t
    val any : 'a constr list -> 'a t
    val require : string -> 'a constr
    val require_bits : string -> string -> 'a constr
  end
end

module type BINDINGS = functor (F : FOREIGN) -> sig end

let view_stage_error = Failure "generated view not available in early stage"

let gen_c prefix fmt : (module FOREIGN) =
  (module
   struct
     type 'a fn = unit
     let foreign cname fn =
       Cstubs_generate_c.fn ~cname ~stub_name:(prefix ^ cname) fmt fn
     let funptr _fn = Ctypes.void

     module Enum = struct
       type 'a constr = string
       type 'a t = 'a constr list
       type from = Cstubs_generate_c.cppdec

       let macro ~from ~type_name ?use_module ?default bits constructors =
         let stub_prefix = Printf.sprintf "%smacro_%s_" prefix type_name in
         Cstubs_generate_c.macro ~from ~stub_prefix fmt constructors;
         Ctypes.view
           ~read:(fun _ -> raise view_stage_error)
           ~write:(fun _ -> raise view_stage_error)
           bits

       let one cl = cl
       let any cl = cl
       let require name = name
       let require_bits ftype name = name
     end
   end)

type bind = Bind : string * string * string * ('a -> 'b) Ctypes.fn -> bind
type macro = {
  bits        : Cstubs_generate_ml.bits;
  default     : string option;
  use_module  : string option;
  type_name   : string;
  stub_prefix : string;
  enum        : Cstubs_generate_ml.enum;
}

let write_foreign fmt bindings =
  ListLabels.iter bindings
    ~f:(fun (Bind (stub_name, _, external_name, fn)) ->
      Cstubs_generate_ml.fn ~stub_name ~external_name fmt fn);

  Format.fprintf fmt
    "let foreign : type a b. string -> (a -> b) Ctypes.fn -> (a -> b) =@\n";
  Format.fprintf fmt
    "  fun name t -> match name, t with@\n@[<v>";
  ListLabels.iter bindings
    ~f:(fun (Bind (_, stub_name, external_name, fn)) ->
      Cstubs_generate_ml.case ~stub_name ~external_name fmt fn);
  Format.fprintf fmt "@[<hov 2>@[|@ s,@ _@ ->@]@ ";
  Format.fprintf fmt " @[@[Printf.fprintf@ stderr@ \"No match for %%s\" s@];";
  Format.fprintf fmt "@ @[assert false@]@]@]@]@."

let write_funptr fmt =
  Format.fprintf fmt "let funptr : ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn Ctypes.typ =@\n";
  Format.fprintf fmt "  fun fn -> Foreign.funptr fn@\n@\n"

let write_enum fmt macros =
  ListLabels.iter macros
    ~f:(fun ({bits; use_module; type_name; stub_prefix; enum}) ->
      Cstubs_generate_ml.macro
        ~type_name ?use_module ~stub_prefix ~bits fmt enum
    );

  Format.fprintf fmt "module Enum = struct@\n";
  Format.fprintf fmt "  type 'a constr = string@\n";
  Format.fprintf fmt "  type 'a t = 'a constr list@\n";
  Format.fprintf fmt
    "  type from = [ `Include of string | `Define of string * string ]@\n@\n";
  Format.fprintf fmt
    "  let macro ~from ~type_name ?use_module ?default bits enum =@\n";
  Format.fprintf fmt
    "    let read, write = match use_module, type_name with@\n";
  ListLabels.iter macros
    ~f:(fun ({use_module; type_name}) ->
      Format.fprintf fmt
        "    | %s, %s -> %s.(of_code ~host, to_code ~host)@\n"
        (match use_module with Some m -> "Some \""^m^"\"" | None -> "None")
        type_name (String.capitalize type_name);
    );
  Format.fprintf fmt
    "    in Ctypes.view ~read ~write bits@\n";
  Format.fprintf fmt
    "  let one cl = cl@\n";
  Format.fprintf fmt
    "  let any cl = cl@\n";
  Format.fprintf fmt
    "  let require name = name@\n";
  Format.fprintf fmt
    "  let require_bits ftype name = name@\n";
  Format.fprintf fmt "end@\n@\n"

let gen_ml prefix fmt : (module FOREIGN) * (unit -> unit) =
  let bindings = ref []
  and macros   = ref []
  and counter  = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  Format.fprintf fmt "type 'a fn = 'a@\n@\n";
  (module
   struct
     type 'a fn = unit
     let foreign cname fn =
       let external_name = var prefix cname
       and stub_name = prefix ^ cname in
       bindings := Bind (stub_name, cname, external_name, fn) :: !bindings
     let funptr _fn = Ctypes.void

     module Enum = struct
       type 'a constr = Cstubs_generate_ml.macro
       type 'a t = Cstubs_generate_ml.enum
       type from = Cstubs_generate_c.cppdec

       let macro ~from ~type_name ?use_module ?default bits enum =
         let stub_prefix = Printf.sprintf "%smacro_%s_" prefix type_name in
         macros := {
           bits =  Cstubs_generate_ml.Bits bits; default;
           use_module; type_name; stub_prefix; enum;
         } :: !macros;
         Ctypes.view
           ~read:(fun _ -> raise view_stage_error)
           ~write:(fun _ -> raise view_stage_error)
           bits

       let one cl = Cstubs_generate_ml.One cl
       let any cl = Cstubs_generate_ml.Any cl
       let require name = Cstubs_generate_ml.Tag name
       let require_bits ftype name = Cstubs_generate_ml.Field (name, ftype)
     end
   end),
  fun () -> begin
    write_enum fmt (List.rev !macros);
    write_funptr fmt; write_foreign fmt !bindings
  end

let write_c fmt ~prefix (module B : BINDINGS) =
  let module M = B((val gen_c prefix fmt)) in ()

let write_ml fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml prefix fmt in
  let module M = B((val foreign)) in finally ()
