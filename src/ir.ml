(* This file is part of tracas.

  tracas is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  tracas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with tracas. If not, see
  <http://www.gnu.org/licenses/>. *)

class ir_instruction addr insn =
  let (address, mnemonic, size) =
    let disa = Disa.disassemble addr insn in
    (disa.Disa.address, disa.Disa.mnemonic, disa.Disa.size)
  in
  object
    val mutable nb = 1

    method get_addr = address
    method incr = nb <- nb + 1
    method dump = (string_of_int address) ^ " " ^ mnemonic ^ " " ^ (string_of_int size) ^ "\n"

  end;;

type trace_element = Insn of ir_instruction | InsnRef of ir_instruction ref

class ir_trace =
object
  val mutable trace = []

  method add_insn addr insn =
    match List.find_opt (
              fun i -> match i with
                         Insn i -> i#get_addr = addr
                       | _ -> false
            ) trace with
    | Some (Insn i) ->
       i#incr;
       trace <- trace @ [InsnRef(ref i)]
    | _             ->
       trace <- trace @ [Insn(new ir_instruction addr insn)]

  method dump = String.concat "" (List.map (fun i -> match i with Insn i -> " " ^ i#dump | InsnRef i -> "*" ^ !i#dump) trace)

end;;






let rec init_trace value =
  let trc = new ir_trace in
  explore trc value
and explore trc = function
  | Json.JSONAssoc (("trace", trace)::_) -> explore_trace trc trace
  | Json.JSONAssoc obj                   -> explore trc (Json.JSONAssoc (List.tl obj))    (* todo: explore info value *)
  | _ -> failwith "cannot init trace (fun explore)\n"
and explore_trace trc = function
  | Json.JSONAssoc l -> List.iter (fun i -> explore_instruction trc i) l; trc
  | _ -> failwith "cannot init trace (fun explore_trace)\n"
and explore_instruction trc = function
  | (addr, Json.JSONAssoc (("insn", Json.JSONString insn)::_)) -> trc#add_insn (int_of_string addr) insn
  | (_, _)                                                     -> () (* todo: get eot and length values *)
