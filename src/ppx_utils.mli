(** Utility methods for manipulation PPX expressions and locations. *)

open Ppxlib

val rocq_loc_of_loc : location -> expression
(** [rocq_loc_of_loc loc] converts a Ppxlib [location] to an expression
    representing a Rocq [Loc.t]. *)

val with_let_bindings : loc:location -> (string loc * bool * expression) list -> expression -> expression
(** [with_let_bindings ~loc bindings expr] wraps expression [expr] with the
    given list of named let-bindings.

    For example, [with_let_bindings ~loc [({ txt = "x"; … }, [%expr 0]); ({ txt = "y"; … }; [%expr 1])] [%expr x + y]]
    generates the code [let x = 0 in let y = 1 in x + y].

    The boolean in the list indicates whether the variable is allowed to be unused. *)

val with_let_patterns : loc:location -> (Ast.pattern * expression) list -> expression -> expression
(** [with_let_patterns ~loc bindings expr] generalizes [with_let_bindings] to allow arbitrary
    patterns on the left-hand side. *)

val gen_symbol : ?prefix:string -> unit -> string
(** [gen_symbol ?prefix ()] generates a new symbol generator whose name starts with
    [prefix]. This is a better-behaved version of [Ppxlib.gen_symbol] that maintains a different
    counter for each prefix. *)
