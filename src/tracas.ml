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
open Cmdliner

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "in file '%s', at line %d, near position %d"
                 pos.pos_fname pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try JsonParser.trace JsonLexer.trace lexbuf with
  | JsonLexer.SyntaxError msg ->
     failwith ("Syntax error (" ^ msg ^ ") " ^ (print_position lexbuf))
  | JsonParser.Error          ->
     failwith ("Syntax error " ^ (print_position lexbuf))


let tracas trc =
  let fd = open_in trc in
  begin
    match parse_with_error (Lexing.from_channel fd) with
    | Some value ->
       let trc = Ir.init_trace value in
       trc#identifying_basic_blocks;
       trc#dump;
       (*trc#dot*)
       ()
    | None -> ()
  end;
  close_in fd


(* command liner *)

let trace =
  let doc = "Specify trace $(docv) (generated by calliope)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let tracas_t = Term.(const tracas $ trace)

let info =
  let doc = "trace analysis" in
  let man = [
      `S Manpage.s_bugs;
      `P "Send issues or PR to <https://github.com/thvnx/tracas>." ]
  in
  Term.info "tracas" ~version:"0" ~doc ~exits:Term.default_exits ~man

let () =
  Term.exit @@ Term.eval (tracas_t, info)
