open! Core
open! Import

let%test_unit "children" =
  Accessor_test_helpers.many (module Sexp) (module Sexp) (module Unit) (fun () ->
    Accessor.Sexp.children)
;;

let%test_unit "tuple2" =
  Accessor_test_helpers.variant
    (module Accessor_test_helpers.Testable.Tuple (Sexp) (Sexp))
    (module Sexp)
    (module Unit)
    (fun () -> Accessor.Sexp.tuple2)
;;

let%test_unit "field" =
  Accessor_test_helpers.many
    (module Sexp)
    (module Sexp)
    (module String)
    Accessor.Sexp.field
;;

let%test_unit "variant" =
  Accessor_test_helpers.optional
    (module Accessor_test_helpers.Testable.List (Sexp))
    (module Sexp)
    (module String)
    Accessor.Sexp.variant
;;

let%test_unit "atoms" =
  Accessor_test_helpers.many (module String) (module Sexp) (module Unit) (fun () ->
    Accessor.Sexp.atoms)
;;

let%test_unit "conv" =
  Accessor_test_helpers.variant (module String) (module Sexp) (module Unit) (fun () ->
    Accessor.Sexp.conv (module String))
;;
