(** Methods for detecting and parsing pattern variables. *)

open Ppxlib

type kind =
  | First_order
  | Second_order

type t =
  { name: Located_string.t;
    kind: kind }

module Ord = struct
  type nonrec t = t
  let compare x y = String.compare x.name.txt y.name.txt
end

module Map = Map.Make(Ord)

module Set = struct
  type t = int Map.t

  let empty = Map.empty

  let add elt s =
    Map.update elt (function Some n -> Some (n + 1) | None -> Some 1) s

  let union s1 s2 =
    Map.merge (fun _ v1 v2 ->
        match v1, v2 with
        | Some n, Some m -> Some (n + m)
        | Some n, None   -> Some n
        | None  , Some m -> Some m
        | None  , None   -> None
      ) s1 s2

   let elements = Map.bindings
end

(* TODO: We perform a very limited logic:
   - We don't check that the first letter is lowercase.
   - We don't warn when the character is supported by Rocq, but not by OCaml. *)
let is_ident_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> true
  | _ -> false

let parse_name ~question_mark string =
  let ident_start = Located_string.advance "?" question_mark in
  let ident_end = Located_string.find_f ~from:ident_start (Fun.negate is_ident_char) string in
  let name = Located_string.substring ~from:ident_start ?until:ident_end string in
  match name.txt with
  | "" -> None
  | _ -> Some (name, ident_end)

let find ~from string =
  let ( let* ) = Option.bind in
  let* question_mark = Located_string.find ~from '?' string in
  (* Check for second-order patterns of the form [@?ident]. *)
  let kind, start_pos =
    match Located_string.previous question_mark with
    | Some pos when Located_string.lookup pos string == '@' ->
       Second_order, pos
    | _ -> First_order, question_mark
  in
  match parse_name ~question_mark string with
  | Some (name, ident_end) ->
     let loc = { loc_start = start_pos.pos; loc_end = name.loc.loc_end; loc_ghost = false } in
     Some ({ name = { txt = name.txt; loc }; kind }, ident_end)
  | None -> None

let rec find_all_from ~from ~acc string =
  match find ~from string with
  | None -> acc
  | Some (pattern, None) -> Set.add pattern acc
  | Some (pattern, Some pattern_end) ->
     find_all_from ~from:pattern_end ~acc:(Set.add pattern acc) string

let find_all ~loc string =
  let string = { txt = string; loc } in
  let start = Located_string.start_position loc in
  find_all_from ~from:start ~acc:Set.empty string
