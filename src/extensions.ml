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

let expand_antiquotation ~name ~tactic_mode parser quasiparser ~ctxt string string_loc =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let template = Template.parse ~loc:string_loc string in
  let runtime_template, antiquotations =
    Template.interpret
      ~loc:string_loc
      ~default:Antiquotations.constr
      ~explicit:Antiquotations.term
      template
  in
  let rocq_loc = Ppx_utils.rocq_loc_of_loc string_loc in
  let parser =
    match antiquotations with
    | [] -> parser
    | _ -> quasiparser
  in
  let parse_result = [%expr [%e parser] ~loc:[%e rocq_loc] [%e runtime_template]] in
  let parse_result =
    if tactic_mode then [%expr Ppx_rocq_runtime.Tactics.memoize [%e parse_result]]
    else parse_result
  in
  let parse_result = Hoister.hoist ~loc ~name parse_result in
  match antiquotations with
  | [] -> parse_result
  | _ ->
     if tactic_mode then
       [%expr Ppx_rocq_runtime.Parsing.substitute
           [%e parse_result]
           [%e Ast_builder.Default.pexp_array ~loc antiquotations]]
     else
       [%expr [%e parse_result] [%e Ast_builder.Default.pexp_array ~loc antiquotations]]

(** {2 [Constrexpr.constr_expr]} *)

module Expr = struct
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let parser = [%expr Ppx_rocq_runtime.Parsing.parse_constrexpr] in
    let quasiparser = [%expr Ppx_rocq_runtime.Parsing.quasiparse_constrexpr] in
    expand_antiquotation ~name:"expr" ~tactic_mode:false parser quasiparser ~ctxt

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
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let parser = [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_string] in
    let quasiparser = [%expr Ppx_rocq_runtime.Parsing.glob_constr_of_quasistring] in
    expand_antiquotation ~name:"preterm" ~tactic_mode:true parser quasiparser ~ctxt

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
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let parser = [%expr Ppx_rocq_runtime.Parsing.constr_of_string] in
    let quasiparser = [%expr Ppx_rocq_runtime.Parsing.constr_of_quasistring] in
    expand_antiquotation ~name:"constr" ~tactic_mode:true parser quasiparser ~ctxt

  let extension =
    Extension.V3.declare
      "constr"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ drop)))
      expand

  let rule = Ppxlib.Context_free.Rule.extension extension
end

module Open_constr = struct
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let parser = [%expr Ppx_rocq_runtime.Parsing.open_constr_of_string] in
    let quasiparser = [%expr Ppx_rocq_runtime.Parsing.open_constr_of_quasistring] in
    expand_antiquotation ~name:"open_constr" ~tactic_mode:true parser quasiparser ~ctxt

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
