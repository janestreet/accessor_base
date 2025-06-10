open! Core
open! Import

let%test_unit "true_" =
  Accessor_test_helpers.variant (module Unit) (module Bool) (module Unit) (fun () ->
    Accessor.Bool.true_)
;;

let%test_unit "false_" =
  Accessor_test_helpers.variant (module Unit) (module Bool) (module Unit) (fun () ->
    Accessor.Bool.false_)
;;

let%test_unit "negated" =
  Accessor_test_helpers.isomorphism (module Bool) (module Bool) (module Unit) (fun () ->
    Accessor.Bool.negated)
;;
