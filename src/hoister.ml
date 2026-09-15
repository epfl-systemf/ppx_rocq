(** AST traversal with hoisting. *)

open Ppxlib

let hoist ~loc ?name e =
  match name with
  | Some name ->
     let name = Ast_builder.Default.estring ~loc name in
     [%expr [%e e] [@hoist [%e name]]]
  | None -> [%expr [%e e] [@hoist]]

let hoisted_expressions_collector =
  let hoist_attribute = Ast_pattern.(attribute ~name:(string "hoist") ~payload:(alt_option (single_expr_payload (estring __)) (pstr nil))) in
  let expr_pattern = Ast_pattern.(pexp_attributes (hoist_attribute ^:: drop) __) in
  object
    inherit [structure_item list] Ast_traverse.fold_map as super

    method! expression expr acc =
      let loc = expr.pexp_loc in
      match Ast_pattern.parse_res expr_pattern loc expr (fun prefix unannotated -> (prefix, unannotated)) with
      | Ok (prefix, expr) ->
         let expr, acc = super#expression expr acc in
         let symbol = Ppx_utils.gen_symbol ?prefix () in
         let variable = Ast_builder.Default.pvar ~loc symbol in
         (* [open (struct … end)] hides our generated bindings from inferred
            signatures, e.g., as printed by [ocamlc -i]. *)
         let binding = [%stri open (struct let [%p variable] = [%e expr] end)] in
         (Ast_builder.Default.evar ~loc symbol, binding :: acc)
      | Error _ ->
         super#expression expr acc
  end

let expand_hoisting structure =
  let expand_item item =
    let new_item, hoisted = hoisted_expressions_collector#structure_item item [] in
    List.rev (new_item :: hoisted)
  in
  List.concat_map expand_item structure
