(************************************************************************)
(* The λΠ-modulo Interactive Proof Assistant                            *)
(************************************************************************)

(************************************************************************)
(* λΠ-modulo serialization Toplevel                                     *)
(* Copyright 2018 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+      *)
(* Written by: Emilio J. Gallego Arias                                  *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

module J = Yojson.Basic

(* Ad-hoc parsing for file:///foo... *)
let parse_uri str =
  let l = String.length str - 7 in
  String.(sub str 7 l)

let mk_reply ~id r = `Assoc [ "jsonrpc", `String "2.0"; "id",     `Int id;   "result", `Assoc r ]
let mk_event m p   = `Assoc [ "jsonrpc", `String "2.0"; "method", `String m; "params", `Assoc p ]

let mk_diagnostic ((p : Pos.pos), (lvl : int), (msg : string)) : J.json =
  let open Pos in
  let range =
    `Assoc ["start", `Assoc ["line", `Int Input.(line_num p.start_buf - 1); "character", `Int Input.(line_offset p.start_buf)];
            "end",   `Assoc ["line", `Int Input.(line_num p.end_buf - 1);   "character", `Int Input.(line_offset p.end_buf)]]
  in
  `Assoc ["range", range;
          "severity", `Int lvl;
          "message",  `String msg]

let mk_diagnostics file version ld : J.json =
  mk_event "textDocument/publishDiagnostics"
    ["uri", `String ("file://"^file);
     "version", `Int version;
     "diagnostics", `List List.(map mk_diagnostic ld)]
