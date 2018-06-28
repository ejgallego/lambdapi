(** Signature for symbols. *)

open Console
open Files
open Terms
open Pos
open Extra

(** Representation of a signature. It roughly corresponds to a set of symbols,
    defined in a single module (or file). *)
type t =
  { symbols : sym StrMap.t ref
  ; path    : module_path
  ; deps    : (string * rule) list PathMap.t ref }

(* NOTE the [deps] field contains a hashtable binding the [module_path] of the
   external modules on which the current signature depends to an association
   list. This association list then maps definable symbols of the external
   module to additional reduction rules defined in the current signature. *)

(** [create path] creates an empty signature with module path [path]. *)
let create : module_path -> t = fun path ->
  { path ; symbols = ref StrMap.empty ; deps = ref PathMap.empty }

(** [find sign name] finds the symbol named [name] in [sign] if it exists, and
    raises the [Not_found] exception otherwise. *)
let find : t -> string -> sym =
  fun sign name -> StrMap.find name !(sign.symbols)

(** [mem sign name] checks whether the symbol named [name] exists in [sign]. *)
let mem : t -> string -> bool =
  fun sign name -> StrMap.mem name !(sign.symbols)

(** [loaded] stores the signatures of the known (already compiled) modules. An
    important invariant is that all the occurrences of a symbol are physically
    equal (even across different signatures). In particular, this requires the
    objects to be copied when loading an object file. *)
let loaded : t PathMap.t ref = ref PathMap.empty

(** [loading] contains the [module_path] of the signatures (or files) that are
    being processed. They are stored in a stack due to dependencies. Note that
    the topmost element corresponds to the current module.  If a [module_path]
    appears twice in the stack, then there is a circular dependency. *)
let loading : module_path list ref = ref []

(** [current_sign ()] returns the current signature. *)
let current_sign () =
  let mp =
    match !loading with
    | mp :: _ -> mp
    | []      -> assert false
  in
  PathMap.find mp !loaded

(** [pp_symbol oc s] prints the name of the symbol [s] to the channel [oc].The
    name is qualified when the symbol is not defined in the current module. *)
let pp_symbol : sym pp = fun oc s ->
  let (path, name) = (s.sym_path, s.sym_name) in
  let sign = current_sign() in
  let full =
    if path = sign.path then name
    else String.concat "." (path @ [name])
  in
  Format.pp_print_string oc full

let _ = Print.pp_symbol_ref := pp_symbol

(** [link sign] establishes physical links to the external symbols. *)
let link : t -> unit = fun sign ->
  let rec link_term t =
    let link_binder b =
      let (x,t) = Bindlib.unbind b in
      Bindlib.unbox (Bindlib.bind_var x (lift (link_term t)))
    in
    match unfold t with
    | Vari(_)     -> t
    | Type        -> t
    | Kind        -> t
    | Symb(s)     -> Symb(link_symb s)
    | Prod(a,b)   -> Prod(link_term a, link_binder b)
    | Abst(a,t)   -> Abst(link_term a, link_binder t)
    | Appl(t,u)   -> Appl(link_term t, link_term u)
    | Meta(_,_)   -> assert false
    | Patt(i,n,m) -> Patt(i, n, Array.map link_term m)
    | TEnv(t,m)   -> TEnv(t, Array.map link_term m)
  and link_rule r =
    let lhs = List.map link_term r.lhs in
    let (xs, rhs) = Bindlib.unmbind r.rhs in
    let rhs = lift (link_term rhs) in
    let rhs = Bindlib.unbox (Bindlib.bind_mvar xs rhs) in
    {r with lhs ; rhs}
  and link_symb s =
    if s.sym_path = sign.path then s else
    try
      let sign = PathMap.find s.sym_path !loaded in
      try find sign s.sym_name with Not_found -> assert false
    with Not_found -> assert false
  in
  let fn _ s =
    Timed.(s.sym_type  := link_term !(s.sym_type));
    Timed.(s.sym_def   := Option.map link_term !(s.sym_def));
    Timed.(s.sym_rules := List.map link_rule !(s.sym_rules))
  in
  StrMap.iter fn !(sign.symbols);
  let gn path ls =
    let sign =
      try PathMap.find path !loaded
      with Not_found -> assert false
    in
    let h (n, r) =
      let r = link_rule r in
      let s = find sign n in
      Timed.(s.sym_rules := !(s.sym_rules) @ [r])
    in
    List.iter h ls
  in
  PathMap.iter gn !(sign.deps)

(** [unlink sign] removes references to external symbols (and thus signatures)
    in the signature [sign]. This function is used to minimize the size of our
    object files, by preventing a recursive inclusion of all the dependencies.
    Note however that [unlink] processes [sign] in place, which means that the
    signature is invalidated in the process. *)
let unlink : t -> unit = fun sign ->
  let unlink_sym s = s.sym_type := Kind; s.sym_rules := [] in
  let rec unlink_term t =
    let unlink_binder b = unlink_term (snd (Bindlib.unbind b)) in
    let unlink_term_env t =
      match t with
      | TE_Vari(_) -> ()
      | _          -> assert false (* Should not happen, matching-specific. *)
    in
    match unfold t with
    | Vari(_)      -> ()
    | Type         -> ()
    | Kind         -> ()
    | Symb(s)      -> if s.sym_path <> sign.path then unlink_sym s
    | Prod(a,b)    -> unlink_term a; unlink_binder b
    | Abst(a,t)    -> unlink_term a; unlink_binder t
    | Appl(t,u)    -> unlink_term t; unlink_term u
    | Meta(_,_)    -> assert false (* Should not happen, uninstantiated. *)
    | Patt(_,_,_)  -> () (* The environment only contains variables. *)
    | TEnv(t,m)    -> unlink_term_env t; Array.iter unlink_term m
  and unlink_rule r =
    List.iter unlink_term r.lhs;
    let (_, rhs) = Bindlib.unmbind r.rhs in
    unlink_term rhs
  in
  let fn _ s =
    unlink_term !(s.sym_type);
    Option.iter unlink_term !(s.sym_def);
    List.iter unlink_rule !(s.sym_rules)
  in
  StrMap.iter fn !(sign.symbols);
  let gn _ ls = List.iter (fun (_, r) -> unlink_rule r) ls in
  PathMap.iter gn !(sign.deps)

(** [add_symbol sign const name a] creates a fresh symbol with the name [name]
    (which should not already be used in [sign]) and with the type [a], in the
    signature [sign]. The created symbol is also returned. *)
let add_symbol : t -> bool -> strloc -> term -> sym = fun sign const name a ->
  let sym_name = name.elt in
  let sym =
    { sym_name ; sym_type = ref a ; sym_path = sign.path ; sym_def = ref None
    ; sym_rules = ref [] ; sym_const = const }
  in
  Timed.(sign.symbols := StrMap.add sym_name sym !(sign.symbols));
  out 3 "(symb) %s\n" sym_name; sym

(** [is_const s] tells whether the symbol is constant. *)
let is_const : sym -> bool = fun s ->
  s.sym_const || (!(s.sym_rules) = [] && !(s.sym_def) = None)

(** [write sign file] writes the signature [sign] to the file [fname]. *)
let write : t -> string -> unit = fun sign fname ->
  match Unix.fork () with
  | 0 -> let oc = open_out fname in
         unlink sign; Marshal.to_channel oc sign [Marshal.Closures];
         close_out oc; exit 0
  | i -> ignore (Unix.waitpid [] i)

(* NOTE [Unix.fork] is used to safely [unlink] and write an object file, while
   preserving a valid copy of the written signature in the parent process. *)

(** [read fname] reads a signature from the object file [fname]. Note that the
    file can only be read properly if it was build with the same binary as the
    one being evaluated. If this is not the case, the program gracefully fails
    with an error message. *)
let read : string -> t = fun fname ->
  let ic = open_in fname in
  try
    let sign = Marshal.from_channel ic in
    close_in ic; sign
  with Failure _ ->
    close_in ic;
    fatal_no_pos "File [%s] is incompatible with current binary...\n" fname

(* NOTE here, we rely on the fact that a marshaled closure can only be read by
   processes running the same binary as the one that produced it. *)

(** [add_rule def r] adds the new rule [r] to the definable symbol [def]. When
    the rule does not correspond to a symbol of the current signature,  it  is
    also stored in the dependencies. *)
let add_rule : t -> sym -> rule -> unit = fun sign sym r ->
  Timed.(sym.sym_rules := !(sym.sym_rules) @ [r]);
  out 3 "(rule) %a\n" Print.pp_rule (sym, r);
  if sym.sym_path <> sign.path then
    let m =
      try PathMap.find sym.sym_path !(sign.deps)
      with Not_found -> assert false
    in
    let deps = PathMap.add sym.sym_path ((sym.sym_name,r)::m) !(sign.deps) in
    Timed.(sign.deps := deps)
