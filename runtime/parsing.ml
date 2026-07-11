(** API for parsing terms. *)

let parse ?loc entry s = Procq.parse_string ?loc entry s

(** Execute function [f] in synterp phase. This function is a hack that tricks
    Rocq by temporarily setting the [Flags.in_synterp] flag. *)
let with_synterp f =
  let old = !Flags.in_synterp_phase in
  Flags.in_synterp_phase := Some true;
  Fun.protect
    ~finally:(fun () -> Flags.in_synterp_phase := old)
    f

(* Entries registered at synterp time. *)
let constrexpr     = with_synterp (fun () -> Procq.eoi_entry Procq.Constr.term)
let ident          = with_synterp (fun () -> Procq.eoi_entry Procq.Constr.ident)
let qualid         = with_synterp (fun () -> Procq.eoi_entry Procq.Prim.qualid)
let match_pattern  = with_synterp (fun () -> Procq.eoi_entry Procq.Constr.cpattern)
let vernac_control = with_synterp (fun () -> Procq.eoi_entry Pvernac.Vernac_.vernac_control)
let ltac           = with_synterp (fun () -> Procq.eoi_entry Ltac_plugin.Pltac.tactic)
let ltac2          = with_synterp (fun () -> Procq.eoi_entry Ltac2_plugin.G_ltac2.ltac2_expr)

let parse_constrexpr ?loc s    = parse ?loc constrexpr s
let parse_ident ?loc s         = parse ?loc ident s
let parse_qualid ?loc s        = parse ?loc qualid s
let parse_vernac ?loc s        = parse ?loc vernac_control s
let parse_ltac ?loc s          = parse ?loc ltac s
let parse_ltac2 ?loc s         = parse ?loc ltac2 s
let parse_match_pattern ?loc s = parse ?loc match_pattern s

let glob_constr_of_string ?loc s =
  let parsed_term = parse_constrexpr ?loc s in
  Terms.Glob_constr.of_constrexpr parsed_term

let constr_of_string ?loc s =
  let parsed_term = parse_constrexpr ?loc s in
  Terms.Constr.of_constrexpr parsed_term

let open_constr_of_string ?loc s =
  let parsed_term = parse_constrexpr ?loc s in
  Terms.Open_constr.of_constrexpr parsed_term

let match_pattern_of_string ?loc s =
  let parsed_pattern = parse_match_pattern ?loc s in
  Terms.Pattern.of_constrexpr parsed_pattern

(** {1 Parsing with antiquotations} *)

(** {2 Generic arguments} *)

type genarg_antiquotation =
  [ `Constr of Terms.constr           (** [%{…}] or [%constr:{…}] *)
  | `Open_constr of Terms.open_constr (** [%open_constr:{…}] *)
  | `Preterm of Terms.glob_constr     (** [%preterm:{…}] *)
  ]

type antiquotation =
  [ genarg_antiquotation
  | `Expr of Terms.constrexpr (** [%expr:{…}] *)
  ]

(** As a performance optimization, we interpret [%preterm:{…}] and
    [%constr:{…}] as generic arguments, so that we don't have to
    re-globalize/re-typecheck the given terms. *)

[%%if rocq = (9, 2)]
let wit_antiquotation : (genarg_antiquotation, genarg_antiquotation, Util.Empty.t) Genarg.genarg_type =
  Genarg.make0 "ppx_rocq:antiquotation"
[%%else]
let wit_antiquotation : (genarg_antiquotation, genarg_antiquotation) GenConstr.tag =
  GenConstr.create "ppx_rocq:antiquotation"
[%%endif]

[%%if rocq = (9, 2)]
let intern_antiquotation ?loc:_ glob_sign (antiquotation: genarg_antiquotation) =
  (* constr and preterm antiquotations are already internalized. *)
  glob_sign, antiquotation

let () =
  Genintern.register_intern0 wit_antiquotation intern_antiquotation
[%%else]
let intern_antiquotation ?loc:_ _glob_sign (antiquotation: genarg_antiquotation) =
  (* constr and preterm antiquotations are already internalized. *)
  antiquotation

let () =
  Genintern.register_intern_constr wit_antiquotation intern_antiquotation
[%%endif]

let interp_constr_antiquotation ?loc env sigma tycon c =
  let judgment = Retyping.get_judgment_of env sigma c in
  match tycon with
  | None -> judgment, sigma
  | Some ty ->
     (* Recheck judgement against the typing condition. *)
     let sigma =
       try Evarconv.unify_leq_delay env sigma judgment.uj_type ty
       with Evarconv.UnableToUnify (sigma, e) ->
         Pretype_errors.error_actual_type ?loc env sigma judgment ty e
     in
     { judgment with uj_type = ty }, sigma

let interp_preterm_antiquotation env sigma tycon t =
  let open Pretyping in
  let tycon =
    match tycon with
    | Some ty -> OfType ty
    | None -> WithoutTypeConstraint
  in
  let sigma, t, ty =
    Pretyping.understand_tcc_ty
      ~flags:(Ltac2_plugin.Tac2core.preterm_flags)
      ~expected_type:tycon
      env sigma t
  in
  Environ.make_judge t ty, sigma

let interp ?loc ~poly:_ env sigma tycon =
  let env = GlobEnv.renamed_env env in
  function
  | `Constr c | `Open_constr c -> interp_constr_antiquotation ?loc env sigma tycon c
  | `Preterm t -> interp_preterm_antiquotation env sigma tycon t

let () =
  GlobEnv.register_constr_interp0 wit_antiquotation interp

(* Module substitution does not affect our antiquotations. *)
[%%if rocq = (9, 2)]
let () = Gensubst.register_subst0 wit_antiquotation (fun _ v -> v)
[%%else]
let () = Gensubst.register_constr_subst wit_antiquotation (fun _ v -> v)
[%%endif]

let print_antiquotation (antiquotation: genarg_antiquotation) =
  let open Pp in
  Genprint.PrinterBasic (fun env sigma ->
    match antiquotation with
    | `Constr c -> str "%{" ++ Printer.pr_econstr_env env sigma c ++ str "}"
    | `Open_constr c -> str "%open_constr:{" ++ Printer.pr_econstr_env env sigma c ++ str "}"
    | `Preterm t -> str "%preterm:{" ++ Printer.pr_glob_constr_env env sigma t ++ str "}"
  )

[%%if rocq = (9, 2)]
let () = Genprint.register_noval_print0 wit_antiquotation print_antiquotation print_antiquotation
[%%else]
let () = Genprint.register_constr_print wit_antiquotation print_antiquotation print_antiquotation
[%%endif]

(** {2 Camlp5 grammar tricks} *)

(** Generic production rule for antiquotations:
    [[ [ "%{"; n = natural; "}" -> { Hole n } ] ]]
 *)
let antiquotation_production =
  let open Procq in
  Production.make
    (Rule.next
       (Rule.next
          (Rule.next (Procq.Rule.stop)
             ((Symbol.token (Tok.PKEYWORD ("%{")))))
          ((Symbol.nterm Prim.natural)))
       ((Symbol.token (Tok.PKEYWORD ("}")))))
    (fun _ n _ loc -> Hole.make ~loc n)

(** Execute function [f] where [entry] allows anti-quotations, which are
    replaced by holes. *)
let with_holes entry f =
  with_synterp (fun () ->
    let grammar_state = Procq.freeze () in
    let () =
      Egramml.grammar_extend ~ignore_kw:false
        entry
        (Reuse (Some "0", [antiquotation_production]))
    in
    Fun.protect
      ~finally:(fun () -> Procq.unfreeze grammar_state)
      f
  )

(** {2 Quasiparsing methods} *)

let parse_with_holes ?loc s =
  with_holes Procq.Constr.term (fun () -> parse_constrexpr ?loc s)

open Proofview.Monad
open Tactics

[%%if rocq = (9, 2)]
let antiquotation_to_constrexpr ?loc : antiquotation -> Terms.constrexpr = function
  | `Expr e -> e
  | #genarg_antiquotation as antiquotation ->
     let genarg = Constrexpr.CGenarg (Genarg.GenArg (Genarg.rawwit wit_antiquotation, antiquotation)) in
     CAst.make ?loc genarg
[%%else]
let antiquotation_to_constrexpr ?loc : antiquotation -> Terms.constrexpr = function
  | `Expr e -> e
  | #genarg_antiquotation as antiquotation ->
     let genarg = Constrexpr.CGenarg (GenConstr.Raw (wit_antiquotation, antiquotation)) in
     CAst.make ?loc genarg
[%%endif]

let quasiparse_constrexpr ?loc s =
  let partial_term = parse_with_holes ?loc s in
  fun substitutions -> Hole.fill_holes
                         (fun ?loc (Hole n) -> antiquotation_to_constrexpr ?loc substitutions.(n))
                         partial_term

[%%if rocq = (9, 2)]
let antiquotation_to_glob_constr ?loc (glob_sign: Genintern.glob_sign) : antiquotation -> Terms.glob_constr = function
  | `Expr e ->
     let env = glob_sign.genv in
     let sigma = Evd.from_env env in
     Constrintern.intern_core WithoutTypeConstraint env sigma glob_sign.intern_sign e
  | `Preterm e -> e
  | (`Constr _ | `Open_constr _) as antiquotation ->
     let open Glob_term in
     let genarg = GGenarg (Genarg.GenArg (Genarg.glbwit wit_antiquotation, antiquotation)) in
     DAst.make ?loc genarg
[%%else]
let antiquotation_to_glob_constr ?loc glob_sign : antiquotation -> Terms.glob_constr = function
  | `Expr e -> Constrintern.intern_core WithoutTypeConstraint glob_sign e
  | `Preterm e -> e
  | (`Constr _ | `Open_constr _) as antiquotation ->
     let open Glob_term in
     let genarg = GGenarg (GenConstr.Glb (wit_antiquotation, antiquotation)) in
     DAst.make ?loc genarg
[%%endif]

let glob_constr_of_quasistring ?loc s =
  let open Tactics in
  let partial_term = parse_with_holes ?loc s in
  let* partial_glob_constr = Terms.Glob_constr.of_constrexpr partial_term in
  return (fun substitutions -> Hole.fill_glob_holes
                         (fun ?loc (Hole n) glob_sign -> antiquotation_to_glob_constr ?loc glob_sign substitutions.(n))
                         partial_glob_constr)

let open_constr_of_quasistring ?loc s =
  let* partial_glob_constr = glob_constr_of_quasistring ?loc s in
  return (fun substitutions ->
      let glob_constr = partial_glob_constr substitutions in
      Terms.Open_constr.of_glob_constr glob_constr)

let constr_of_quasistring ?loc s =
  let* partial_glob_constr = glob_constr_of_quasistring ?loc s in
  return (fun substitutions ->
      let glob_constr = partial_glob_constr substitutions in
      Terms.Constr.of_glob_constr glob_constr)

let substitute term_with_holes values =
  let* t = term_with_holes in t values
