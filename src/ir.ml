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
  let (address, mnemonic, size, jump) =
    let disa = Disa.disassemble addr insn in
    (disa.Disa.address, disa.Disa.mnemonic, disa.Disa.size, disa.Disa.jump)
  in
  object
    val mutable nb = 1
    val mutable pred:(ir_instruction list) = []
    val mutable succ:(ir_instruction list) = []
    val mutable lead = false

    method jump_p = jump
    method set_lead = lead <- true
    method add_succ s = if not (List.mem s succ) then succ <- s :: succ
    method add_pred p = if not (List.mem p pred) then pred <- p :: pred

    method get_addr = address
    method incr = nb <- nb + 1
    method dump =
      let zto_string l =
        String.concat "," (List.map (fun i -> string_of_int i#get_addr) l)
      in
      Printf.sprintf "%d %s %d %d %s %s %s %b\n" address mnemonic nb size (if lead then "\t:L" else "")
                    (zto_string pred) (zto_string succ) jump

  end;;

type trace_element = Insn of ir_instruction | Duplicate of ir_instruction

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
       trace <- trace @ [Duplicate(i)]
    | _             ->
       trace <- trace @ [Insn(new ir_instruction addr insn)]

  method dump = String.concat "" (List.map (fun i -> match i with Insn i -> " " ^ i#dump | Duplicate i -> "*" ^ i#dump) trace)

  method identifying_basic_blocks =
    (* 1/ find leaders, then dertemine BB.
         Leaders are: The first instruction in the trace, and Any instruction that
         immediately follows a conditional or unconditional jump, or return.
       2/ build blocks -> [leader; insn before next leader] *)
    let rec process = function
        Insn x      :: Insn y :: t
      | Duplicate x :: Insn y :: t -> x#add_succ y;
                                      if x#jump_p then y#set_lead;
                                      y#add_pred x;
                                      process (Insn(y)::t)

      | Insn x      :: Duplicate y :: t
      | Duplicate x :: Duplicate y :: t -> x#add_succ y;
                                           if x#jump_p then y#set_lead;
                                           y#add_pred x;
                                           process (Duplicate(y)::t)

      | Insn _ :: [] | Duplicate _ :: [] | [] -> ()
    in
    let _ = match (List.hd trace) with
        Insn i | Duplicate i -> i#set_lead in
    process trace


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
