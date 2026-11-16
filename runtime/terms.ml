(** Term API. *)

open Proofview.Monad
open Tactics

(** {1 Term representations} *)

type constrexpr = Constrexpr.constr_expr
type glob_constr = Glob_term.glob_constr
type constr = EConstr.constr
type open_constr = EConstr.t
type pattern = Pattern.constr_pattern

(** {1 Conversions} *)

module Expr = struct
  type t = constrexpr

  let map f c =
    Constrexpr_ops.map_constr_expr_with_binders
      (fun _ () -> ())
      (fun () -> f)
      ()
      c
end

module Glob_constr = struct
  type t = glob_constr

  let of_constrexpr ?(pattern = false) e =
    let env = Global.env () in
    let sigma = Evd.from_env env in
    if pattern then Constrintern.intern_gen WithoutTypeConstraint ~pattern_mode:true env sigma e
    else Constrintern.intern_constr env sigma e

  let map = Glob_ops.map_glob_constr
end

[%%if rocq <= (9, 2)]
let merge_ustate = Evd.merge_universe_context
[%%else]
let merge_ustate = Evd.merge_ustate
[%%endif]

module Constr = struct
  type t = constr

  let of_glob_constr c =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let constr, ustate = Pretyping.understand env sigma c in
    let sigma = merge_ustate sigma ustate in
    Proofview.Unsafe.tclEVARS sigma >>
    return constr
end

module Open_constr = struct
  type t = open_constr

  let of_glob_constr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let sigma, econstr = Pretyping.understand_tcc env sigma e in
    Proofview.Unsafe.tclEVARS sigma >>
    return econstr
end

module Pattern = struct
  type t = pattern

  [%%if rocq >= (9, 2)]
  let of_constrexpr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let _, pattern = Constrintern.interp_constr_pattern env sigma e in
    return pattern
  [%%else]
  let of_constrexpr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let _, pattern = Constrintern.intern_constr_pattern env sigma e in
    return pattern
  [%%endif]

  [%%if rocq >= (9, 2)]
  let of_glob_constr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let _, uninstantiated_pattern = Patternops.pattern_of_glob_constr env e in
    let ltac_map = Glob_ops.empty_lvar in
    let pattern = Patternops.interp_pattern env sigma ltac_map uninstantiated_pattern in
    return pattern
  [%%else]
  let of_glob_constr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let _, pattern = Patternops.pattern_of_glob_constr env e in
    return pattern
  [%%endif]

  let wildcard = return (Pattern.PMeta None)
end
