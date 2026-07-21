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

  let of_glob_constr c =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let flags = (PrintingFlags.current ()).extern in
    let extern_env = Constrextern.extern_env ~flags env sigma in
    return (Constrextern.extern_glob_constr extern_env c)

  let of_constr c =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let flags = PrintingFlags.current () in
    return (Constrextern.extern_constr ~flags env sigma c)

  let map f c =
    Constrexpr_ops.map_constr_expr_with_binders
      (fun _ () -> ())
      (fun () -> f)
      ()
      c
end

module Glob_constr = struct
  type t = glob_constr

  let of_constrexpr e =
    let env = Global.env () in
    let sigma = Evd.from_env env in
    Constrintern.intern_constr env sigma e

  let of_constr c =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let flags = (PrintingFlags.current ()).detype in
    return (Detyping.detype Detyping.Now ~flags env sigma c)

  let map = Glob_ops.map_glob_constr
  let fold = Glob_ops.fold_glob_constr
end

[%%if rocq = (9, 2)]
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

  let of_constrexpr e = of_glob_constr (Glob_constr.of_constrexpr e)
end

module Open_constr = struct
  type t = open_constr

  let of_glob_constr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let sigma, econstr = Pretyping.understand_tcc env sigma e in
    Proofview.Unsafe.tclEVARS sigma >>
    return econstr

  let of_constrexpr e =
    of_glob_constr (Glob_constr.of_constrexpr e)
end

module Pattern = struct
  type t = pattern

  let of_constrexpr e =
    let* env = Tactics.env in
    let* sigma = Tactics.evar_map in
    let _, pattern = Constrintern.interp_constr_pattern env sigma e in
    return pattern

  let wildcard = return (Pattern.PMeta None)
end
