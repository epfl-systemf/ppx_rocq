(** Methods for detecting and parsing pattern variables. *)

open Ppxlib

(** Kind of pattern variables. *)
type kind =
  | First_order  (** [?name] *)
  | Second_order (** [@?name] *)

(** Type of pattern variables. *)
type t = private
  { name: Located_string.t; (** Name of the pattern variable. *)
    kind: kind              (** Kind of the pattern variable. *)
  }

(** Multi-sets of pattern variables. *)
module Set : sig
  type t
  type elt

  val empty : t
  (** [empty] returns an empty set of pattern variables. *)

  val add : elt -> t -> t
  (** [add var s] adds an occurrence of the pattern variable to the set. *)

  val union : t -> t -> t
  (** [union s1 s2] takes the union of two sets of pattern variables,
      adding the occurrences of each variable. *)

  val elements : t -> (elt * int) list
  (** [elements s] returns the list of elements of [s] with their number of occurrences. *)
end with type elt := t

val find_all : loc:location -> string -> Set.t
(** [find_all ~loc string] finds all pattern variables in [string]. *)
