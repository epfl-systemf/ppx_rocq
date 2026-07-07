(** Support for pattern matching on terms and goals. *)

open Ltac2_plugin
open Tac2match
open Names

(** {1 Matching over terms} *)

type 'a case = match_pattern Proofview.tactic * 'a continuation
(** Type of cases in term matching. *)

and 'a continuation = context -> substitution -> 'a Proofview.tactic
(** Type of continuations in a pattern matching branch. *)

and substitution = Ltac_pretype.patvar_map
(** Type of substitutions. *)

val match_term : EConstr.constr -> 'a case list -> 'a Proofview.tactic
(** [match_term t cases] performs pattern matching on term [t] with
    backtracking, i.e., if a branch fails, the next branch is [tried]. *)

val lazy_match_term : EConstr.constr -> 'a case list -> 'a Proofview.tactic
(** [lazy_match_term t cases] performs pattern matching on term [t], committing
    to the first branch that matches. *)

val multi_match_term : EConstr.constr -> 'a case list -> 'a Proofview.tactic
(** [multi_match_term t cases] performs pattern matching on term [t] just like
    [match_term t], but additionally if a tactic fails after the [match], the
    next branch is tried. *)

(** {1 Matching over goals} *)

type 'a goal_case = match_rule Proofview.tactic * ((Id.t * context * context) array -> 'a continuation)
(** Type of cases in a goal pattern-matching expression.

    The continuation takes an array of hypotheses with their names, binder context, and type contexts,
    as well as the context for the conclusion and the pattern-variable substitution map. *)

val match_goal : ?reverse:bool -> Evd.econstr -> 'a goal_case list -> 'a Proofview.tactic
(** [match_goal ?reverse t cases] performs goal matching on [t] with backtracking,
    i.e., if a branch tactic fails, the next branch is tried. *)

val lazy_match_goal : ?reverse:bool -> Evd.econstr -> 'a goal_case list -> 'a Proofview.tactic
(** [lazy_match_goal ?reverse t cases] performs goal matching on [t],
    committing to the first branch that matches. *)

val multi_match_goal : ?reverse:bool -> Evd.econstr -> 'a goal_case list -> 'a Proofview.tactic
(** [multi_match_goal ?reverse t ~cases] performs goal matching on [t] like [match_goal],
    but additional if a tactic fails after the [match], the next branch is tried. *)
