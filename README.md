# `ppx_rocq`: Syntax extensions for quoting Rocq terms in OCaml

`ppx_rocq` is a PPX rewriter that enables Rocq plugin writers to write Rocq terms using a simple quotation system:

```ocaml
let nat_plus_assoc = {%constr| forall x y z : nat, (x + y) + z = x + (y + z) |} ;;
- : EConstr.t Proofview.tactic
```

`ppx_rocq` is notably used by [Camltac](https://github.com/epfl-systemf/camltac), where it provides:

1. Quotations from Ltac2 (`%constr`, `%open_constr`, `%preterm`), an extra quotation for concrete syntax terms (`%expr`), and quotations for identifiers (`%ident`) and qualifiers (`%qualid`). 

2. Anti-quotations, using the `%{…}` notation:

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
   ```

## Setup

`ppx_rocq` supports Rocq ≥ 9.0, and can be installed through `opam` using the following commands:
```bash
opam repo add rocq-released https://rocq-prover.github.io/opam/released/
opam install ppx_rocq
```

To use `ppx_rocq` in a plugin, add `ppx_rocq` to the `preprocessing` field of your `library` or `executable` Dune stanza:

```dune
(library
 (name my_library)
 ; …
 (preprocessing (pps ppx_rocq)))
```



