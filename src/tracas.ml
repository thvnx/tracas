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

open Lexing

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "in file '%s', at line %d, near position %d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try JsonParser.trace JsonLexer.trace lexbuf with
  | JsonLexer.SyntaxError msg -> Printf.fprintf stderr "Syntax error (%s) %s.\n" msg (print_position lexbuf);
                                 exit 1
  | JsonParser.Error          -> Printf.fprintf stderr "Syntax error %s.\n" (print_position lexbuf);
                                 exit 2


let _ =
  let trace = "../calliope/trace.json" in
  let fd = open_in trace in
  begin
    match parse_with_error (Lexing.from_channel fd) with
    | Some value ->
       Printf.printf "%s" (Ir.init_trace value)#dump
    | None -> ()
  end;
  close_in fd
