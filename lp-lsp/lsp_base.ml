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

(* Whether to send extended lsp messages *)
let std_protocol = ref true

module J = Yojson.Basic

let mk_extra l = if !std_protocol then [] else l

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
    `Assoc ["start", `Assoc ["line", `Int Input.(line_num p.start_buf - 1); "character", `Int Input.(line_offset p.start_buf)];
            "end",   `Assoc ["line", `Int Input.(line_num p.end_buf - 1);   "character", `Int Input.(line_offset p.end_buf)]]
  in
  `Assoc (mk_extra ["goal_fg", goal] @
          ["range", range;
           "severity", `Int lvl;
           "message",  `String msg;
          ])

let mk_diagnostics file version ld : J.json =
  let extra = mk_extra ["version", `Int version] in
  mk_event "textDocument/publishDiagnostics" @@
    extra @
    ["uri", `String ("file://"^file);
     "diagnostics", `List List.(map mk_diagnostic ld)]
