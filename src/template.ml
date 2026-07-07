(** Methods for parsing template strings with antiquotations. *)

open Ppxlib

type 'a fragment =
  | Literal of string
  | Antiquotation of 'a

let rec parse_from ~from string =
  match Antiquotation.find ~from string with
  | Some ({ percent; closing_brace; _ } as antiquotation) ->
     let { txt = literal; _ } = Located_string.substring ~from ~until:percent string in
     let rest =
       match closing_brace with
       | Some closing_brace -> parse_from ~from:(Located_string.advance "}" closing_brace) string
       | None -> []
     in
     let result = Antiquotation antiquotation :: rest in
     if literal <> "" then Literal literal :: result else result
  | None ->
     let { txt = literal; _ } = Located_string.substring ~from string in
     if literal <> "" then [Literal literal] else []

let parse ~loc string =
  let start = Located_string.start_position loc in
  parse_from ~from:start { txt = string; loc }

type interpretation_result =
  | Constant of string
  | Antiquoted_value of expression
  | Unknown_antiquotation of { warning: string loc; antiquotation: string }

let interpret_fragment ~default ~explicit fragment =
  match fragment with
  | Literal lit -> Constant lit
  | Antiquotation antiquotation ->
     match Antiquotation.interpret_expression ~default ~explicit antiquotation with
     | Ok value -> Antiquoted_value value
     | Error warning -> Unknown_antiquotation { warning; antiquotation = Antiquotation.to_string antiquotation }

let interpolate ~loc fragments =
  (* TODO: Replace it with named tuples for OCaml >= 5.4 *)
  let open struct
    type t = { parts: expression list;
               warnings: string loc list }
  end in
  let default ~loc e = [%expr ([%e e] : string)] in
  let rec interpolate fragments =
    match fragments with
    | [] -> { parts = []; warnings = [] }
    | fragment :: fragments' ->
       let result = interpolate fragments' in
       match interpret_fragment ~default ~explicit:[] fragment with
       | Constant lit ->
          let expr = Ast_builder.Default.estring ~loc lit in
          { result with parts = expr :: result.parts }
       | Antiquoted_value expr ->
          { result with parts = expr :: result.parts }
       | Unknown_antiquotation { warning; antiquotation } ->
          let expr = Ast_builder.Default.estring ~loc antiquotation in
          { parts = expr :: result.parts;
            warnings = warning :: result.warnings }
  in
  let { parts; warnings } = interpolate fragments in
  let concatenated = [%expr String.concat "" [%e Ast_builder.Default.elist ~loc parts]] in
  Ast_diagnostics.warn' warnings concatenated

let interpret ~loc ~default ~explicit fragments =
  (* TODO: Replace it with named tuples for OCaml >= 5.4 *)
  let open struct
    type t = { template: string;
               antiquotations: expression list;
               warnings: string loc list }
  end in
  let rec interpret fragments next_id =
    match fragments with
    | [] ->
       { template = ""; antiquotations = []; warnings = [] }
    | fragment :: fragments' ->
       match interpret_fragment ~default ~explicit fragment with
       | Constant lit ->
          let result = interpret fragments' next_id in
          { result with template = lit ^ result.template }
       | Antiquoted_value expr ->
          let result = interpret fragments' (next_id + 1) in
          { result with template = "%{" ^ string_of_int next_id ^ "}" ^ result.template;
                        antiquotations = expr :: result.antiquotations }
       | Unknown_antiquotation { warning; antiquotation } ->
          let result = interpret fragments' next_id in
          { result with template = antiquotation ^ result.template;
                        warnings = warning :: result.warnings }
  in
  let { template; antiquotations; warnings } = interpret fragments 0 in
  let runtime_template = Ast_builder.Default.estring ~loc template in
  let runtime_template = Ast_diagnostics.warn' warnings runtime_template in
  runtime_template, antiquotations
