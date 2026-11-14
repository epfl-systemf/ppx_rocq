(lang dune 3.17)

(generate_opam_files true)
(subst disabled) ;; Work-around for Dune 3.17 subst bug: https://github.com/ocaml/dune/issues/11045

(name ppx_rocq)
(version 1.0.0)
(authors "Dario Halilovic")
(maintainers "Dario Halilovic")
(license LGPL-2.1-only)

(homepage "https://github.com/epfl-systemf/ppx_rocq")
(bug_reports "https://github.com/epfl-systemf/ppx_rocq/issues")
(source (github epfl-systemf/ppx_rocq))

(package
 (name ppx_rocq)
 (synopsis "PPX syntax extensions for quoting Rocq terms in OCaml")
 (description "`ppx_rocq` is a PPX rewriter that enables Rocq plugin writers \
to write Rocq terms using a simple quotation system:
```ocaml
let nat_plus_assoc = {%constr| forall x y z : nat, (x + y) + z = x + (y + z) |} ;;
- : EConstr.t Proofview.tactic
```

`ppx_rocq` is notably used by [Camltac](https://github.com/epfl-systemf/camltac), \
where it provides:

1. Quotations from Ltac2 (`%constr`, `%open_constr`, `%preterm`), an extra \
quotation for concrete syntax terms (`%expr`), and quotations for identifiers \
(`%ident`) and qualifiers (`%qualid`).

2. Anti-quotations, using the `\%{…}` notation:
   ```ocaml
   let lhs = {%expr| (x + y) + z |} in
   let rhs = {%expr| x + (y + z) |} in
   let nat_plus_assoc = {%constr| forall x y z : nat, %expr:{lhs} = %expr:{rhs} |} ;;
   - : EConstr.t Proofview.tactic
   ```

3. Pattern-matching extensions for matching over terms and goals:
   ```ocaml
   match%rocq {| 1 + 1 |} with
   | {| ?x + _ |} -> Proofview.tclUNIT x
   | _ -> assert false ;;
   - : EConstr.t Proofview.tactic
   ```")
 (tags
  ("category:Miscellaneous/Rocq Extensions"
   "keyword:ocaml"
   "keyword:ppx"))
 (depends
   (ocaml (>= 4.14))
   (coq-core (>= 9.0))
   (dune (< 3.24)) ;; Dune 3.24 removes "%{coq:version}" variables
   ppxlib
   ppx_optcomp))
