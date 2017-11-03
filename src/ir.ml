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

type value =
  | JSONAssoc  of (string * value) list
  | JSONBool   of bool
  | JSONFloat  of float
  | JSONInt    of int
  | JSONList   of value list
  | JSONNull
  | JSONString of string

let rec output_value = function
  | JSONAssoc obj  -> print_assoc obj
  | JSONList l     -> print_list l
  | JSONString s   -> Printf.sprintf "\"%s\"" s
  | JSONInt i      -> Printf.sprintf "%d" i
  | JSONFloat x    -> Printf.sprintf "%f" x
  | JSONBool true  -> Printf.sprintf "true"
  | JSONBool false -> Printf.sprintf "false"
  | JSONNull       -> Printf.sprintf "null"

and print_assoc obj =
  Printf.sprintf "{ %s }" (
                   String.concat ", " (
                                   List.map (
                                       fun (key, value) ->
                                       Printf.sprintf "\"%s\": %s" key (output_value value)
                                     ) obj
                                 )
                 )
and print_list arr =
  Printf.sprintf "[ %s ]" (
                   String.concat ", " (
                                   List.map (
                                       fun value ->
                                       Printf.sprintf "%s" (output_value value)
                                     ) arr
                                 )
                 )
