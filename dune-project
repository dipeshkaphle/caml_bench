(lang dune 3.8)

(name caml_bench)

(generate_opam_files true)

(authors "Dipesh Kafle")

(maintainers "Dipesh Kafle")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name caml_bench)
 (synopsis "Benchmarking library for OCaml")
 (description "Simple benchmarking library for OCaml")
 (depends ocaml dune sqlite3)
 (tags
  (benchmarking ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
