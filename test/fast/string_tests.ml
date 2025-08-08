open! Core
open! Import

let%test_unit "list" =
  Accessor_test_helpers.isomorphism
    (module Accessor_test_helpers.Testable.List (Char))
    (module String)
    (module Unit)
    (fun () -> Accessor.String.list)
;;

let%test_unit "reversed" =
  Accessor_test_helpers.isomorphism
    (module String)
    (module String)
    (module Unit)
    (fun () -> Accessor.String.reversed)
;;

let%test_unit "each" =
  Accessor_test_helpers.many (module Char) (module String) (module Unit) (fun () ->
    Accessor.String.each)
;;

let%test_unit "prefixed" =
  Accessor_test_helpers.variant
    (module String)
    (module String)
    (module String)
    Accessor.String.prefixed
;;

let%test_unit "suffixed" =
  Accessor_test_helpers.variant
    (module String)
    (module String)
    (module String)
    Accessor.String.suffixed
;;
