(** Methods for parsing antiquotations. *)

open Ppxlib

type kind =
  | Default                      (** [%{…}] *)
  | Explicit of Located_string.t (** [%kind:{…}] *)

type t =
  { percent: Located_string.position;
    kind: kind;
    opening_brace: Located_string.position;
    expression: Located_string.t;
    closing_brace: Located_string.position option }

let parse_kind ~percent ~opening_brace string =
  let kind_start = Located_string.advance "%" percent in
  let kind = Located_string.substring ~from:kind_start ~until:opening_brace string in
  match kind.txt with
  | "" -> Some Default
  | k when String.ends_with ~suffix:":" k ->
     let name = String.sub k 0 (String.length k - 1) in
     let is_valid_char c =
       match c with
       | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
       | _ -> false
     in
     if name <> "" && String.for_all is_valid_char name then
       let colon_pos = { opening_brace.pos with pos_cnum = opening_brace.pos.pos_cnum - 1 } in
       let loc = { loc_start = kind.loc.loc_start; loc_end = colon_pos; loc_ghost = false } in
       Some (Explicit { txt = name; loc })
     else
       None
  | _ -> None

(** [find ~from string] finds the first antiquotation after position [from]
    inside the string [string] *)
let rec find ~from string =
  let ( let* ) = Option.bind in
  let* percent = Located_string.find ~from '%' string in
  let* opening_brace = Located_string.find ~from:percent '{' string in
  match parse_kind ~percent ~opening_brace string with
  | None ->
     (* False positive, so retry. *)
     find ~from:(Located_string.advance "%" percent) string
  | Some kind ->
     let closing_brace = Located_string.find ~from:opening_brace '}' string in
     let expression_start = Located_string.advance "{" opening_brace in
     let expression = Located_string.substring ~from:expression_start ?until:closing_brace string in
     Some { percent; kind; opening_brace; expression; closing_brace }

let parse_expression expression =
  let { txt = string; loc } = expression in
  let lexbuf = Lexing.from_string string in
  Lexing.set_position lexbuf loc.loc_start;
  Lexing.set_filename lexbuf loc.loc_start.pos_fname;
  try Parse.expression lexbuf
  with _ ->
    (* TODO: Report the parsing error. *)
    Ast_diagnostics.error ~loc "Could not parse expression %S" string

let interpret_expression ~default ~explicit { percent; kind; expression; closing_brace; _ } =
  (* Check that the expression is closed before parsing. *)
  let parse_expression expression =
    match closing_brace with
    | Some _pos -> parse_expression expression
    | None ->
       let loc = { loc_start = percent.pos; loc_end = expression.loc.loc_end; loc_ghost = false } in
       let hint = "Hint: close the antiquotation with '}'" in
       Ast_diagnostics.error ~loc "Unclosed antiquotation\n%s" hint
  in
  match kind with
  | Default ->
     (* The location given to the [default] function is the location of the
        "%{" part. *)
     let loc = { loc_start = percent.pos; loc_end = expression.loc.loc_start; loc_ghost = false } in
     let expression = parse_expression expression in
     Ok (default ~loc expression)
  | Explicit { txt = kind; loc } ->
     (* Find the interpretation function in [explicit]. *)
     match List.assoc_opt kind explicit with
     | Some f ->
        let expression = parse_expression expression in
        Ok (f ~loc expression)
     | None ->
        let suggestion = Spellcheck.spellcheck (List.map fst explicit) kind in
        match suggestion with
        | Some hint ->
           Error { txt = "Unknown antiquotation \"" ^ kind ^ "\"\n" ^ hint; loc }
        | None ->
           Error { txt = "Unknown antiquotation \"" ^ kind ^ "\""; loc }

let to_string { kind; expression; _ } =
  "%"
  ^ (match kind with Default -> "" | Explicit { txt; _ } -> txt ^ ":")
  ^ "{"
  ^ expression.txt
  ^ "}"
