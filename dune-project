(lang dune 3.17)

(generate_opam_files true)

(name ppx_rocq)
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
   rocq-runtime
   ppxlib
   ppx_optcomp))
