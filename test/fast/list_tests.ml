open! Core
open! Import
module Bool_list = Accessor_test_helpers.Testable.List (Bool)

let%test_unit "nil" =
  Accessor_test_helpers.variant
    (module Unit)
    (module Bool_list)
    (module Unit)
    (fun () -> Accessor.List.nil)
;;

let%test_unit "cons" =
  Accessor_test_helpers.variant
    (module Accessor_test_helpers.Testable.Tuple (Bool) (Bool_list))
    (module Bool_list)
    (module Unit)
    (fun () -> Accessor.List.cons)
;;

let%test_unit "nth" =
  Accessor_test_helpers.optional
    (module Bool)
    (module Bool_list)
    (module Int)
    Accessor.List.nth
;;

let%test_unit "reversed" =
  Accessor_test_helpers.isomorphism
    (module Bool_list)
    (module Bool_list)
    (module Unit)
    (fun () -> Accessor.List.reversed)
;;

let%test_unit "prefixed" =
  Accessor_test_helpers.variant
    (module Bool_list)
    (module Bool_list)
    (module Bool_list)
    (Accessor.List.prefixed ~equal:Bool.equal)
;;

let%test_unit "suffixed" =
  Accessor_test_helpers.variant
    (module Bool_list)
    (module Bool_list)
    (module Bool_list)
    (Accessor.List.suffixed ~equal:Bool.equal)
;;

let%test_unit "each" =
  Accessor_test_helpers.many
    (module Bool)
    (module Bool_list)
    (module Unit)
    (fun () -> Accessor.List.each)
;;
