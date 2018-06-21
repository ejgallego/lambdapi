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

let json_of_goal g =
  let open Yojson.Basic in
  let pr_hyp (s,(_,t)) =
    `Assoc ["hname", `String s;
            "htype", `String (Format.asprintf "%a" Print.pp_term (Bindlib.unbox t))] in
  let j_env = List.map pr_hyp g.Proofs.g_hyps in
  `Assoc ["hyps", `List j_env;
          "type", `String (Format.asprintf "%a" Print.pp_term g.Proofs.g_type)]

let json_of_thm thm =
  let open Proofs in
  match thm with
  | None ->
    `Null
  | Some thm ->
    json_of_goal thm.t_focus

let mk_diagnostic ((p : Pos.pos), (lvl : int), (msg : string), (thm : Proofs.theorem option)) : J.json =
  let open Pos in
  let goal = json_of_thm thm in
  let range =
    `Assoc ["start", `Assoc ["line", `Int (p.start_line - 1); "character", `Int p.start_col];
            "end",   `Assoc ["line", `Int (p.end_line - 1);   "character", `Int p.end_col]]
  in
  `Assoc ["range", range;
          "severity", `Int lvl;
          "message",  `String msg;
          "goal_fg", goal]

let mk_diagnostics file version ld : J.json =
  mk_event "textDocument/publishDiagnostics"
    ["uri", `String ("file://"^file);
     "version", `Int version;
     "diagnostics", `List List.(map mk_diagnostic ld)]
