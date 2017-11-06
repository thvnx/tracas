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

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF

%start <Json.value option> trace
%%

trace:
  | EOF       { None }
  | v = value { Some v }
  ;

value:
  | LEFT_BRACE; obj = object_fields; RIGHT_BRACE
    { Json.JSONAssoc obj }
  | LEFT_BRACK; vl = array_values; RIGHT_BRACK
    { Json.JSONList vl }
  | s = STRING
    { Json.JSONString s }
  | i = INT
    { Json.JSONInt i }
  | x = FLOAT
    { Json.JSONFloat x }
  | TRUE
    { Json.JSONBool true }
  | FALSE
    { Json.JSONBool false }
  | NULL
    { Json.JSONNull }
  ;

object_fields: obj = rev_object_fields { List.rev obj };

rev_object_fields:
  | { [] }
  | k = STRING; COLON; v = value { [(k, v)] }
  | obj = rev_object_fields; COMMA; k = STRING; COLON; v = value { (k, v) :: obj }
  ;

array_values:
  | { [] }
  | vl = rev_values { List.rev vl }
  ;

rev_values:
  | v = value { [v] }
  | vl = rev_values; COMMA; v = value { v :: vl }
  ;
