(** Extensions using OCaml's [match] construct. *)

open Ppxlib

(** {1 Definitions} *)

(** Type of patterns in a term-matching case. *)
type term_pattern =
  | Term of pattern_expr                        (** Matches the whole term. *)
  | Context of pattern_expr * string loc option (** e.g. [Context "?x + ?y" as c] *)
and pattern_expr =
  | Pattern of string loc       (** e.g. ["?x + ?y"] *)
  | Wildcard of location option (** [_] *)

(** Type of patterns for matching hypotheses. *)
type hypothesis_pattern =
  { name: label loc;              (** Name of the hypothesis. *)
    binder_pattern: term_pattern; (** Pattern to apply to the binder. *)
    type_pattern: term_pattern    (** Pattern to apply to the type. *)
  }

(** Type of patterns in a goal-matching case. *)
type goal_pattern =
  { hypotheses: hypothesis_pattern list;
    conclusion: term_pattern }

(** Type of pattern-matching cases. *)
type 'pattern match_case =
  { pattern: 'pattern;  (** Pattern condition of the case. *)
    rhs: expression loc (** Expression to execute when the pattern matches. *)
  }

(** Type of scrutinees of the term pattern-matching constructs. *)
type match_term_scrutinee =
  | Literal of string loc    (** ["…"] *)
  | Expression of expression (** Any expression of type [constr]. *)

(** Type of term-matching expressions. *)
type match_term_expression =
  { scrutinee: match_term_scrutinee;     (** Scrutinee of the [match] expression. *)
    cases: term_pattern match_case list; (** List of cases of the [match]. *)
  }

(** Type of goal-matching expressions. *)
type match_goal_expression =
  { reverse: bool;                       (** Whether the match should be reversed or not. *)
    cases: goal_pattern match_case list; (** List of cases of the [match]. *)
  }

(** Type of pattern-matching variants. *)
type match_key =
  | Default (** Default backtracking pattern-matching. *)
  | Lazy    (** Non-backtracking. *)
  | Multi   (** Backtracking even after the match. *)

(** {2 AST patterns} *)

(** Term-matching patterns.

    Example: ["?x + ?y"], [_]. *)
let pattern_expr (): (pattern, pattern_expr -> 'a, 'a) Ast_pattern.t =
  let regular_pattern = Ast_pattern.(
      map ~f:(fun f s loc -> f (Pattern { txt = s; loc })) @@
        ppat_constant (pconst_string __ __ drop))
  in
  let wildcard_pattern = Ast_pattern.(
      map ~f:(fun f loc -> f (Wildcard (Some loc))) @@
        ppat_loc __ ppat_any)
  in
  Ast_pattern.alt regular_pattern wildcard_pattern

(** Term- or subterm-matching patterns.

    Example: ["?x + ?y"], [Context "?x + ?y" as c]. *)
let term_pattern (): (pattern, term_pattern -> 'a, 'a) Ast_pattern.t =
  let context_pattern () = Ast_pattern.(
      ppat_construct
        (lident (string "Context"))
        (some (pair nil (pattern_expr ()))))
  in
  let context_pattern = Ast_pattern.(
     alt
       (map ~f:(fun f pat label -> f (pat, Some label)) (ppat_alias (context_pattern ()) __'))
       (map ~f:(fun f pat -> f (pat, None)) (context_pattern ())))
  in
  Ast_pattern.(
    alt
      (map ~f:(fun f (pat, label) -> f (Context (pat, label))) context_pattern)
      (map ~f:(fun f pat -> f (Term pat)) (pattern_expr ())))

(** Term pattern-matching cases.

    Example: ["?x + ?y" -> …]. *)
let match_term_case =
  let match_term_case = Ast_pattern.(
      case
        ~lhs:(term_pattern ())
        ~guard:none
        ~rhs:__')
  in
  Ast_pattern.(map ~f:(fun f pattern rhs -> f { pattern; rhs }) match_term_case)

(** Term pattern-matching expressions.

    Example: [match%constr c with "?x + ?y" -> …]. *)
let match_term: (expression, match_term_expression -> expression, expression) Ast_pattern.t =
  let match_term = Ast_pattern.(pexp_match __' (many match_term_case)) in
  let literal_scrutinee =
    Ast_pattern.(
      alt
        (pexp_extension (extension (string "constr") (single_expr_payload (estring __')))) (* [%constr "…"] *)
        (estring __')) (* "…" *)
  in
  Ast_pattern.(map ~f:(fun f { txt = scrutinee; loc } cases ->
                   (* Check whether the scrutinee is a constant string. *)
                   let x = Ast_pattern.parse_res literal_scrutinee loc scrutinee (fun x -> x) in
                   match x with
                   | Ok string -> f { scrutinee = Literal string; cases }
                   | Error _ -> f { scrutinee = Expression scrutinee; cases }) match_term)

(** Hypothesis pattern.

    Example: [h = "?x + ?y"], [h = _ :: "nat"]. *)
let hypothesis_pattern =
  let name = Ast_pattern.(loc (lident __')) in
  let binder = term_pattern () in
  let binder_with_type =
    Ast_pattern.(ppat_construct (lident (string "::"))
                   (some (pair nil (ppat_tuple (term_pattern () ^:: term_pattern () ^:: nil))))) in
  let rhs =
    Ast_pattern.(
      alt
        (map ~f:(fun f pattern -> f (pattern, Term (Wildcard None))) binder)
        (map ~f:(fun f binder typ -> f (binder, typ)) binder_with_type)
    )
  in
  let hypothesis_pattern = Ast_pattern.pair name rhs in
  Ast_pattern.(map ~f:(fun f name (binder_pattern, type_pattern) -> f { name; binder_pattern; type_pattern }) hypothesis_pattern)

(** Pattern for a list of hypotheses.

    Example: [{ h1 = "?x + ?y"; h2 = "?z" }], [_]. *)
let hypotheses_pattern =
  let hypotheses_pattern = Ast_pattern.(ppat_record (many hypothesis_pattern) closed) in
  Ast_pattern.(alt hypotheses_pattern (map ~f:(fun f -> f []) ppat_any))

(** Goal pattern.

    Example: [{ h1 = "?x + ?y"; h2 = "?z" }, "?x = ?z"]. *)
let goal_pattern =
  let conclusion_pattern = term_pattern () in
  let goal_pattern = Ast_pattern.(ppat_tuple (hypotheses_pattern ^:: conclusion_pattern ^:: nil)) in
  Ast_pattern.(map ~f:(fun f hypotheses conclusion -> f { hypotheses; conclusion }) goal_pattern)

(** Case of a [match%goal] construct.

    Example: [{ h = "?x" :: "nat" }, "?x = ?x" -> …]. *)
let match_goal_case =
  let match_goal_case = Ast_pattern.(
      case
        ~lhs:goal_pattern
        ~guard:none
        ~rhs:__')
  in
  Ast_pattern.(map ~f:(fun f pattern rhs -> f { pattern; rhs }) match_goal_case)

(** Goal pattern-matching construct.

    Example: [match%rocq goal with { H = "?x" :: "nat" }, "?x = ?x" -> …]. *)
let match_goal : (expression, match_goal_expression -> expression, expression) Ast_pattern.t =
  let keyword name = Ast_pattern.(pexp_ident (lident (string name))) in
  let reverse = Ast_pattern.(
      alt
        (map ~f:(fun f -> f true) @@
           pexp_apply
             (keyword "reverse")
             ((pair nolabel (keyword "goal")) ^:: nil))
        (map ~f:(fun f -> f false) (keyword "goal")))
  in
  let match_goal = Ast_pattern.(pexp_match reverse (many match_goal_case)) in
  Ast_pattern.(map ~f:(fun f reverse cases -> f { reverse; cases }) match_goal)

type match_expression =
  | MatchTerm of match_term_expression
  | MatchGoal of match_goal_expression

(** A term-matching or goal-matching expression. *)
let match_expression =
  Ast_pattern.(
    alt
      (map ~f:(fun f expr -> f (MatchTerm expr)) match_term)
      (map ~f:(fun f expr -> f (MatchGoal expr)) match_goal))

(** {1 Expansions} *)

(** {2 Term matching} *)

let pattern_expr pattern =
  match pattern with
  | Term pattern -> pattern
  | Context (pattern, _) -> pattern

let context_var pattern =
  match pattern with
  | Term _ -> None
  | Context (_, var) -> var

let pattern_variables pattern =
  match pattern_expr pattern with
  | Pattern string -> Pattern_variable.find_all ~loc:string.loc string.txt

  | Wildcard _ -> Pattern_variable.Set.empty

module Term = struct
  let expand_pattern ~loc pattern =
    match pattern with
    | Pattern { txt = pattern; loc = pattern_loc } ->
       let pattern_expr = Ast_builder.Default.estring ~loc:pattern_loc pattern in
       let rocq_loc = Ppx_utils.rocq_loc_of_loc pattern_loc in
       Hoister.hoist ~loc ~name:"pattern"
         [%expr
           Ppx_rocq_runtime.Tactics.memoize begin
             Ppx_rocq_runtime.Parsing.match_pattern_of_string
               ~loc:[%e rocq_loc] [%e pattern_expr]
           end]
    | Wildcard wildcard_loc ->
       let loc = match wildcard_loc with Some loc -> loc | None -> loc in
       [%expr Ppx_rocq_runtime.Terms.Pattern.wildcard]

  let expand_term_pattern ~loc pattern =
    match pattern with
    | Term pattern ->
       let expr = expand_pattern ~loc pattern in
       [%expr let* pattern = [%e expr] in Proofview.tclUNIT (Ltac2_plugin.Tac2match.MatchPattern pattern)]
    | Context (pattern, _) ->
       let expr = expand_pattern ~loc pattern in
       [%expr let* pattern = [%e expr] in Proofview.tclUNIT (Ltac2_plugin.Tac2match.MatchContext pattern)]

  let expand_rhs ~loc ~context_var ~pattern_variables rhs =
    let context =
      match context_var with
      | Some label -> Ast_builder.Default.ppat_var ~loc:label.loc label
      | None -> Ast_builder.Default.ppat_any ~loc
    in
    match Pattern_variable.Set.elements pattern_variables with
    | [] ->
       [%expr fun [%p context] _ -> [%e rhs.txt]]
    | pattern_variables ->
       let to_binding Pattern_variable.{ name; _ } =
         let loc = name.loc in
         let name_expr = Ast_builder.Default.estring ~loc name.txt in
         name, [%expr Names.(Id.Map.find (Id.of_string [%e name_expr]) __subst)]
       in
       let bindings = List.map to_binding pattern_variables in
       [%expr fun [%p context] __subst -> [%e Ppx_utils.with_let_bindings ~loc bindings rhs.txt]]

  let expand_case ~loc { pattern; rhs } =
    let lhs = expand_term_pattern ~loc pattern in
    let pattern_variables = pattern_variables pattern in
    let context_var = context_var pattern in
    let rhs = expand_rhs ~loc ~context_var ~pattern_variables rhs in
    [%expr ([%e lhs], [%e rhs])]

  let expand_match_key ~loc key =
    match key with
    | Default -> [%expr Ppx_rocq_runtime.Pattern_matching.match_term]
    | Lazy -> [%expr Ppx_rocq_runtime.Pattern_matching.lazy_match_term]
    | Multi -> [%expr Ppx_rocq_runtime.Pattern_matching.multi_match_term]

  let expand_match ~ctxt key { scrutinee; cases } =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    (* TODO: Warn on any case after a wildcard in [lazy_match], as they're unreachable. *)
    let match_key = expand_match_key ~loc key in
    let cases =
      cases
      |> List.map (expand_case ~loc)
      |> Ast_builder.Default.elist ~loc
    in
    match scrutinee with
    | Expression scrutinee ->
       [%expr [%e match_key] [%e scrutinee] [%e cases]]
    | Literal scrutinee ->
       let scrutinee = Ast_builder.Default.estring ~loc:scrutinee.loc scrutinee.txt in
       [%expr let* __scrutinee = [%constr [%e scrutinee]] in [%e match_key] __scrutinee [%e cases]]
end

(** {2 Goal matching} *)

let goal_pattern_variables goal_pattern =
  let union = Pattern_variable.Set.union in
  let vars =
    goal_pattern.hypotheses
    |> List.map (fun hyp -> union
                              (pattern_variables hyp.binder_pattern)
                              (pattern_variables hyp.type_pattern))
    |> List.fold_left union Pattern_variable.Set.empty
  in
  union vars (pattern_variables goal_pattern.conclusion)

let context_var_pattern ~loc pattern =
  match context_var pattern with
  | Some name -> Ast_builder.Default.ppat_var ~loc:name.loc name
  | None -> Ast_builder.Default.ppat_any ~loc

module Goal = struct

  (* Binder patterns interpret wildcard _ as matching any hypothesis, whether
     they have a body or not. Hence it is slightly different from "_", which matches
     hypotheses with _any_ body. *)
  let expand_binder_pattern ~loc binder_pattern =
    match binder_pattern with
    | Term (Wildcard wildcard_loc) ->
       let loc = match wildcard_loc with Some loc -> loc | None -> loc in
       [%expr Proofview.tclUNIT None]
    | _ ->
       let pattern = Term.expand_term_pattern ~loc binder_pattern in
       [%expr let* value = [%e pattern] in Proofview.tclUNIT (Some value)]

  let expand_hypothesis ~loc { binder_pattern; type_pattern; _ } =
    let binder_pattern = expand_binder_pattern ~loc binder_pattern in
    let type_pattern = Term.expand_term_pattern ~loc type_pattern in
    [%expr
      let* binder = [%e binder_pattern] in
      let* typ = [%e type_pattern] in
      Proofview.tclUNIT (binder, typ)]

  let expand_goal_pattern ~loc { hypotheses; conclusion } =
    let hypotheses = List.map (expand_hypothesis ~loc) hypotheses in
    let conclusion = Term.expand_term_pattern ~loc conclusion in
    [%expr
       let* hypotheses = Ppx_rocq_runtime.Tactics.of_list [%e Ast_builder.Default.elist ~loc hypotheses] in
       let* conclusion = [%e conclusion] in
       Proofview.tclUNIT (hypotheses, conclusion)
    ]

  let expand_rhs ~loc goal_pattern rhs =
    let pattern_variables = goal_pattern_variables goal_pattern in
    let context_var = context_var goal_pattern.conclusion in
    let rhs = Term.expand_rhs ~loc ~context_var ~pattern_variables rhs in
    (** Generate let bindings of the form [let (h, c1, c2) = __hyps.(i)] for hypotheses. *)
    let hyp_binding i hyp =
      let hyp_pattern =
        [%pat?
         [%p Ast_builder.Default.ppat_var ~loc:hyp.name.loc hyp.name],
         [%p context_var_pattern ~loc hyp.binder_pattern],
         [%p context_var_pattern ~loc hyp.type_pattern]]
      in
      hyp_pattern, [%expr __hyps.([%e Ast_builder.Default.eint ~loc i])]
    in
    let bindings = List.mapi hyp_binding goal_pattern.hypotheses in
    [%expr fun __hyps -> [%e Ppx_utils.with_let_patterns ~loc bindings rhs]]

  let expand_case ~loc { pattern; rhs } =
    let lhs = expand_goal_pattern ~loc pattern in
    let rhs = expand_rhs ~loc pattern rhs in
    [%expr ([%e lhs], [%e rhs])]

  let expand_match_key ~loc key =
    match key with
    | Default -> [%expr Ppx_rocq_runtime.Pattern_matching.match_goal]
    | Lazy -> [%expr Ppx_rocq_runtime.Pattern_matching.lazy_match_goal]
    | Multi -> [%expr Ppx_rocq_runtime.Pattern_matching.multi_match_goal]

  let expand_match ~ctxt key { reverse; cases } =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    (* TODO: Warn on any case after a wildcard in [lazy_match], as they're
       unreachable. *)
    let match_key = expand_match_key ~loc key in
    let cases =
      cases
      |> List.map (expand_case ~loc)
      |> Ast_builder.Default.elist ~loc
    in
    let reverse = Ast_builder.Default.ebool ~loc reverse in
    [%expr
        Proofview.Goal.enter_one (fun __goal ->
          [%e match_key]
            ~reverse:[%e reverse]
            (Proofview.Goal.concl __goal)
            [%e cases]
    )]
end

(** {1 Context-free rules} *)

let rule name key =
  let extension =
    Extension.V3.declare
      name
      Extension.Context.expression
      Ast_pattern.(single_expr_payload match_expression)
      (fun ~ctxt payload ->
        match payload with
        | MatchTerm expr -> Term.expand_match ~ctxt key expr
        | MatchGoal expr -> Goal.expand_match ~ctxt key expr)
  in Ppxlib.Context_free.Rule.extension extension

module Rocq = struct let rule = rule "rocq" Default end
module Lazy = struct let rule = rule "lazy" Lazy end
module Multi = struct let rule = rule "multi" Multi end
