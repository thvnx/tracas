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

module CS = Capstone

type insn = { id: int; address: int; size: int; bytes: int array;
              mnemonic: string; op_str: string;
	      regs_read: int array; regs_write: int array;
	      groups: int array;(* arch: CS.cs_arch;*) }

let format insn =
  let rec split_bytes ?acc:(acc = []) i =
    if String.length i >= 2 then
      let byte = Char.chr (int_of_string ("0x" ^ (String.sub i 0 2))) in
      split_bytes ~acc:(acc @ [byte]) (String.sub i 2 ((String.length i) - 2))
    else
      acc
  in
  let bytes = split_bytes (String.sub insn 2 ((String.length insn) - 2)) in
  String.init (List.length bytes) (fun i -> List.nth bytes i)

let disassemble addr insn =
  (* code example: "\x21\x7c\x02\x9b\x21\x7c\x00\x53\x00\x40\x21\x4b\xe1\x0b\x40\xb9"*)
  let code = format insn in
  let (arch, mode, code, syntax) = (CS.CS_ARCH_ARM64, [CS.CS_MODE_ARM], code, 0L) in
  let i =
    let handle = CS.cs_open arch mode in
    (
      if syntax != 0L then
        (
          let err = CS.cs_option handle CS.CS_OPT_SYNTAX syntax in
          match err with
          | _ -> failwith "error"; (* todo: be more specific *)
        );
      let err = CS.cs_option handle CS.CS_OPT_DETAIL CS._CS_OPT_ON in
      match err with
      | _ -> ();
      (*Printf.printf "*************\n";
      List.iter ( fun insn ->
                  Printf.printf "%s 0x%x\t%s\t%s\n" code insn.CS.address insn.CS.mnemonic insn.CS.op_str
                ) (CS.cs_disasm handle code 0x1000L 0L);*)
      let infos = List.hd (CS.cs_disasm handle code 0x1000L 0L) in
      match CS.cs_close handle with
      | 0 -> infos;
      | _ -> failwith "Failed to close handle"; (* todo: be more specific *)

    )
  in { id = i.CS.id; address = addr; size = i.CS.size; bytes = i.CS.bytes;
       mnemonic = i.CS.mnemonic; op_str = i.CS.op_str;
       regs_read = i.CS.regs_read; regs_write = i.CS.regs_write;
       groups = i.CS.groups; }
