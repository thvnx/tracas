let open_subgraph id =
  Printf.sprintf "subgraph cluster_%s {" id

let close_subgraph name =
  Printf.sprintf "label=\"%s\";\n}" name

let open_graph =
  "digraph TBP {"

let close_graph =
  "}"

let node id name =
  Printf.sprintf "\"%s\" [label=\"%s\"];" id name

let edge src dst label =
  Printf.sprintf "\"%s\" -> \"%s\" [label=\"%s\"];" src dst label
