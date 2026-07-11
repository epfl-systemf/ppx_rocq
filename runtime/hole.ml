(** Placeholder values for parsing with incomplete information. *)

(** {1 Definitions} *)

type hole = Hole of int

(* We capture the globalization environment, so that constrexprs substituted
   later can be internalized correctly. *)
type glob_hole = hole * Genintern.glob_sign

(** Pretty-printing representation of holes. *)
let hole_name (Hole n) = "__ppx_hole__" ^ string_of_int n

(** {1 Generic argument} *)

(** We treat holes as a generic term with a delayed interpretation. *)

[%%if rocq = (9, 2)]
let wit_hole : (hole, glob_hole, Util.Empty.t) Genarg.genarg_type = Genarg.make0 "ppx_rocq:hole"
[%%else]
let wit_hole : (hole, glob_hole) GenConstr.tag = GenConstr.create "ppx_rocq:hole"
[%%endif]

(** {2 Internalization} *)

[%%if rocq = (9, 2)]
let () =
  let intern_hole ?loc:_ glob_sign hole = glob_sign, (hole, glob_sign) in
  Genintern.register_intern0 wit_hole intern_hole
[%%else]
let () =
  let intern_hole ?loc:_ glob_sign hole = hole, glob_sign in
  Genintern.register_intern_constr wit_hole intern_hole
[%%endif]

(** {2 Interpretation} *)

let () =
  let interp_hole ?loc:_ ~poly:_ _glob_env _sigma _tycon (Hole _, _) =
    failwith "Cannot interpret hole"
  in
  GlobEnv.register_constr_interp0 wit_hole interp_hole

(** {2 Module substitution} *)

[%%if rocq = (9, 2)]
let () = Gensubst.register_subst0 wit_hole (fun _ v -> v)
[%%else]
let () = Gensubst.register_constr_subst wit_hole (fun _ v -> v)
[%%endif]

(** {2 Printing} *)

let print_hole hole = Genprint.PrinterBasic (fun _env _sigma -> Pp.str (hole_name hole))
let print_glob_hole (hole, _) = print_hole hole

[%%if rocq = (9, 2)]
let () = Genprint.register_noval_print0 wit_hole print_hole print_glob_hole
[%%else]
let () = Genprint.register_constr_print wit_hole print_hole print_glob_hole
[%%endif]

(** {2 Constructors, destructors} *)

[%%if rocq = (9, 2)]
let make ?loc n =
  CAst.make ?loc (Constrexpr.CGenarg (Genarg.GenArg (Rawwit wit_hole, Hole n)))
[%%else]
let make ?loc n =
  CAst.make ?loc (Constrexpr.CGenarg (Raw (wit_hole, Hole n)))
[%%endif]

[%%if rocq = (9, 2)]
let get_raw : type raw glob. (raw, glob, Util.Empty.t) Genarg.genarg_type -> Genarg.raw_generic_argument -> raw option =
  fun t (Genarg.GenArg (Rawwit tag, value)) ->
    match Genarg.genarg_type_eq t tag with
    | Some Refl -> Some value
    | None -> None

let get_glob : type raw glob. (raw, glob, Util.Empty.t) Genarg.genarg_type -> Genarg.glob_generic_argument -> glob option =
  fun t (Genarg.GenArg (Glbwit tag, value)) ->
    match Genarg.genarg_type_eq t tag with
    | Some Refl -> Some value
    | None -> None

[%%else]
let get_raw : type raw glob. (raw, glob) GenConstr.tag -> GenConstr.raw -> raw option =
  fun t (Raw (tag, value)) ->
    match GenConstr.eq t tag with
    | Some Refl -> Some value
    | None -> None

let get_glob : type raw glob. (raw, glob) GenConstr.tag -> GenConstr.glb -> glob option =
  fun t (Glb (tag, value)) ->
    match GenConstr.eq t tag with
    | Some Refl -> Some value
    | None -> None
[%%endif]

let find_hole t =
  match t with
  | Constrexpr.CGenarg raw -> get_raw wit_hole raw
  | _ -> None

let rec fill_holes f t =
  match find_hole t.CAst.v with
  | Some hole -> f ?loc:t.loc hole
  | None -> Terms.Expr.map (fill_holes f) t

let find_glob_hole t =
  match t with
  | Glob_term.GGenarg raw -> get_glob wit_hole raw
  | _ -> None

let rec fill_glob_holes f t =
  match find_glob_hole (DAst.get t) with
  | Some (Hole n, glob_sign) -> f ?loc:t.loc (Hole n) glob_sign
  | None -> Terms.Glob_constr.map (fill_glob_holes f) t
