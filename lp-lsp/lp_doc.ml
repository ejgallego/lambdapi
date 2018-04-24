(************************************************************************)
(* The λΠ-modulo Interactive Proof Assistant                            *)
(************************************************************************)

(************************************************************************)
(* λΠ-modulo serialization arguments                                    *)
(* Copyright 2018 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+      *)
(* Written by: Emilio J. Gallego Arias                                  *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

module LSP = Lsp_base

type ast = Parser.p_cmd Pos.loc

type doc_node = {
  ast  : ast;
  exec : bool;
}

(* Private. A doc is a list of nodes for now. The first element in
   the list is assumed to be the tip of the document. The initial
   document is the empty list.
*)
type doc = {
  nodes : doc_node list;
}

let mk_error file version pos msg =
  LSP.mk_diagnostics file version [pos, 1, msg]

let parse_text contents =
  let open Earley in
  Parser.(parse_string cmd_list blank contents)

let process_cmd _file (st,dg) node =
  let open Pos         in
  let open Handle.Pure in
  match handle_command st node with
  | OK st ->
    st, (node.pos, 3, "OK") :: dg
  | Error msg ->
    st, (node.pos, 1, msg) :: dg

let new_doc modname =
  let open Sign in
  let path = [modname] in
  loading := path :: !loading;
  let sign = create path in
  loaded := Files.PathMap.add path sign !loaded;
  ()

let close_doc modname =
  let open Sign in
  let path = [modname] in
  loaded := Files.PathMap.remove path !loaded;
  loading := List.tl !loading;
  ()

let check_text file version contents =
  try
    let open Handle.Pure in
    let doc = parse_text contents in
    let _st, diag = List.fold_left (process_cmd file) (initial_state,[]) doc in
    LSP.mk_diagnostics file version @@ List.fold_left (fun acc (pos,lvl,msg) ->
        match pos with
        | None     -> acc
        | Some pos -> (pos,lvl,msg) :: acc
      ) [] diag
  with
  | Earley.Parse_error(buf,pos) ->
    let loc = Pos.locate buf pos buf pos in
    mk_error file version loc "Parse error."
