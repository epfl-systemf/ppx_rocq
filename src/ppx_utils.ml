(** Utility methods for manipulation PPX expressions and locations. *)

open Ppxlib

let rocq_loc_of_loc loc =
  let Location.{ loc_start; loc_end; _ } = loc in
  let file         = Ast_builder.Default.estring ~loc loc_start.pos_fname in
  let line_nb      = Ast_builder.Default.eint ~loc loc_start.pos_lnum in
  let line_nb_last = Ast_builder.Default.eint ~loc loc_end.pos_lnum in
  let bol_pos      = Ast_builder.Default.eint ~loc loc_start.pos_bol in
  let bol_pos_last = Ast_builder.Default.eint ~loc loc_end.pos_bol in
  let bp           = Ast_builder.Default.eint ~loc loc_start.pos_cnum in
  let ep           = Ast_builder.Default.eint ~loc loc_end.pos_cnum in
  [%expr
    Loc.{ fname    = InFile { dirpath = None; file = [%e file] };
      line_nb      = [%e line_nb];
      line_nb_last = [%e line_nb_last];
      bol_pos      = [%e bol_pos];
      bol_pos_last = [%e bol_pos_last];
      bp           = [%e bp];
      ep           = [%e ep];
    }
  ]

let with_let_bindings ~loc bindings expr =
  let rec with_let_bindings = function
    | [] -> expr
    | (name, binding) :: rest ->
       let expr = with_let_bindings rest in
       let name = Ast_builder.Default.ppat_var ~loc:name.loc name in
       [%expr let [%p name] = [%e binding] in [%e expr]]
  in
  with_let_bindings bindings

let with_let_patterns ~loc bindings expr =
  let rec with_let_patterns = function
    | [] -> expr
    | (pattern, binding) :: rest ->
       let expr = with_let_patterns rest in
       [%expr let [%p pattern] = [%e binding] in [%e expr]]
  in
  with_let_patterns bindings

let gen_symbol =
  let counts = ref [] in
  fun ?(prefix = "_x") () -> begin
    match List.assoc_opt prefix !counts with
    | Some count ->
       let n = !count in
       count := n + 1;
       Printf.sprintf "%s__%03i_" prefix n
    | None ->
       let count = ref 1 in
       counts := (prefix, count) :: !counts;
       Printf.sprintf "%s__000_" prefix
  end
