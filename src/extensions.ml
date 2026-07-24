(** Extension nodes for quoting Rocq terms. *)

open Ppxlib

(** {1 Extensions with string interpolation} *)

(** Extensions [[%ident]], [[%qualid]], [[%vernac]] support a limited subset of
    antiquotations in the form of string interpolation. *)

let expand_string_interpolation ~ctxt interpolator string string_loc =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let template = Template.parse ~loc:string_loc string in
  let string_expr = Template.interpolate ~loc:string_loc template in
  let rocq_loc = Ppx_utils.rocq_loc_of_loc string_loc in
  [%expr [%e interpolator] ~loc:[%e rocq_loc] [%e string_expr]]

module Ident = struct
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    expand_string_interpolation ~ctxt [%expr Ppx_rocq_runtime.Parsing.parse_ident]

  let extension =
    Extension.V3.declare
      "ident"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

module Qualid = struct
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    expand_string_interpolation ~ctxt [%expr Ppx_rocq_runtime.Parsing.parse_qualid]

  let extension =
    Extension.V3.declare
      "qualid"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

module Vernac = struct
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    expand_string_interpolation ~ctxt [%expr Ppx_rocq_runtime.Parsing.parse_vernac]

  let extension =
    Extension.V3.declare
      "vernac"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

(** {1 Extensions with antiquotations} *)

(** Extensions [[%expr]], [[%glob_constr]], and [[%constr]] support term
    antiquotations. *)

module Antiquotations = struct
  let constr ~loc expr = [%expr `Constr ([%e expr] : Ppx_rocq_runtime.Terms.constr)]
  let open_constr ~loc expr = [%expr `Open_constr ([%e expr] : Ppx_rocq_runtime.Terms.open_constr)]
  let preterm ~loc expr = [%expr `Preterm ([%e expr] : Ppx_rocq_runtime.Terms.glob_constr)]
  let expr ~loc expr = [%expr `Expr ([%e expr] : Ppx_rocq_runtime.Terms.constrexpr)]

  let term = [
      ("constr", constr);
      ("open_constr", open_constr);
      ("preterm", preterm);
      ("expr", expr);
    ]
end

let expand_with_antiquotations ~default_kind ~parse ~quasiparse ~ctxt string string_loc =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let template = Template.parse ~loc:string_loc string in
  let runtime_template, antiquotations =
    Template.interpret
      ~loc:string_loc
      ~default:default_kind
      ~explicit:Antiquotations.term
      template
  in
  let rocq_loc = Ppx_utils.rocq_loc_of_loc string_loc in
  match antiquotations with
  | [] -> parse ~loc ~string_loc ~rocq_loc  ~string:runtime_template
  | _ ->
     let antiquotations = Ast_builder.Default.pexp_array ~loc antiquotations in
     quasiparse ~loc ~string_loc ~rocq_loc ~string:runtime_template ~antiquotations

(** {2 [Constrexpr.constr_expr]} *)

module Expr = struct
  let expand =
    expand_with_antiquotations
      ~default_kind:Antiquotations.expr
      ~parse:(fun ~loc ~string_loc:_ ~rocq_loc ~string ->
        [%expr Ppx_rocq_runtime.Parsing.parse_constrexpr ~loc:[%e rocq_loc] [%e string]]
        |> Hoister.hoist ~loc ~name:"expr"
      )
      ~quasiparse:(fun ~loc ~string_loc:_ ~rocq_loc ~string ~antiquotations ->
        [%expr Ppx_rocq_runtime.Parsing.constrexpr_of_quasistring ~loc:[%e rocq_loc] [%e string]]
        |> Hoister.hoist ~name:"expr" ~loc
        |> fun t -> [%expr Ppx_rocq_runtime.Parsing.substitute_in_constrexpr [%e t] [%e antiquotations]]
      )

  let extension =
    Extension.V3.declare
      "expr"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

(** {2 [Glob_term.glob_constr]} *)

module Preterm = struct
  let expand =
    expand_with_antiquotations
      ~default_kind:Antiquotations.preterm
      ~parse:(fun ~loc ~string_loc ~rocq_loc ~string ->
        [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_string ~loc:[%e rocq_loc] [%e string]]
        |> Persistent_objects.persist ~loc ~string_loc
        |> Hoister.hoist ~loc ~name:"preterm"
      )
      ~quasiparse:(fun ~loc ~string_loc ~rocq_loc ~string ~antiquotations ->
        [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_quasistring ~loc:[%e rocq_loc] [%e string]]
        |> Persistent_objects.persist ~loc ~string_loc
        |> Hoister.hoist ~loc ~name:"preterm"
        |> fun t -> [%expr Ppx_rocq_runtime.Parsing.substitute_in_glob_constr [%e t] [%e antiquotations]]
      )

  let extension =
    Extension.V3.declare
      "preterm"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

(** {2 [EConstr.constr] and [EConstr.t]} *)

module Constr = struct
  let expand =
    expand_with_antiquotations
      ~default_kind:Antiquotations.constr
      ~parse:(fun ~loc ~string_loc ~rocq_loc ~string ->
        [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_string ~loc:[%e rocq_loc] [%e string]]
        |> Persistent_objects.persist ~loc ~string_loc
        |> Hoister.hoist ~loc ~name:"preterm"
        |> fun t -> [%expr Ppx_rocq_runtime.Terms.Constr.of_glob_constr [%e t]]
        |> fun t -> [%expr Ppx_rocq_runtime.Tactics.memoize [%e t]]
        |> Hoister.hoist ~loc ~name:"constr"
      )
      ~quasiparse:(fun ~loc ~string_loc ~rocq_loc ~string ~antiquotations ->
        [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_quasistring ~loc:[%e rocq_loc] [%e string]]
        |> Persistent_objects.persist ~loc ~string_loc
        |> Hoister.hoist ~loc ~name:"preterm"
        |> fun t -> [%expr Ppx_rocq_runtime.Parsing.substitute_in_glob_constr [%e t] s]
        |> fun t -> [%expr Ppx_rocq_runtime.Terms.Constr.of_glob_constr [%e t]]
        |> fun body -> [%expr fun s -> [%e body]]
        |> Hoister.hoist ~loc ~name:"constr"
        |> fun f -> [%expr [%e f] [%e antiquotations]]
      )

  let extension =
    Extension.V3.declare
      "constr"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

module Open_constr = struct
  let expand =
    expand_with_antiquotations
      ~default_kind:Antiquotations.open_constr
      ~parse:(fun ~loc ~string_loc ~rocq_loc ~string ->
        [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_string ~loc:[%e rocq_loc] [%e string]]
        |> Persistent_objects.persist ~loc ~string_loc
        |> Hoister.hoist ~loc ~name:"preterm"
        |> fun t -> [%expr Ppx_rocq_runtime.Terms.Open_constr.of_glob_constr [%e t]]
        |> Hoister.hoist ~loc ~name:"open_constr"
      )
      ~quasiparse:(fun ~loc ~string_loc ~rocq_loc ~string ~antiquotations ->
        [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_quasistring ~loc:[%e rocq_loc] [%e string]]
        |> Persistent_objects.persist ~loc ~string_loc
        |> Hoister.hoist ~loc ~name:"preterm"
        |> fun t -> [%expr Ppx_rocq_runtime.Parsing.substitute_in_glob_constr [%e t] s]
        |> fun t -> [%expr Ppx_rocq_runtime.Terms.Open_constr.of_glob_constr [%e t]]
        |> fun body -> [%expr fun s -> [%e body]]
        |> Hoister.hoist ~loc ~name:"open_constr"
        |> fun f -> [%expr [%e f] [%e antiquotations]]
      )

  let extension =
    Extension.V3.declare
      "open_constr"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

(**/**)

let () =
  Ppxlib.Driver.register_transformation
    ~rules:[
      Ident.rule;
      Qualid.rule;
      Vernac.rule;

      Expr.rule;
      Preterm.rule;
      Constr.rule;
      Open_constr.rule;

      Match_extensions.Rocq.rule;
      Match_extensions.Lazy.rule;
      Match_extensions.Multi.rule;
    ]
    ~impl:(Hoister.expand_hoisting)
    "ppx_rocq"
