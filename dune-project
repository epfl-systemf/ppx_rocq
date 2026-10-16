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
 (depends
   (ocaml (>= 4.14))
   (coq-core (>= 9.0))
   (dune (< 3.24)) ;; Dune 3.24 removes "%{coq:version}" variables
   ppxlib
   ppx_optcomp))
