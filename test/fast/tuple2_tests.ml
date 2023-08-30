open! Core
open! Import

let%test_unit "fst" =
  Accessor_test_helpers.field
    (module Bool)
    (module Accessor_test_helpers.Testable.Tuple (Bool) (Bool))
    (module Unit)
    (fun () -> Accessor.Tuple2.fst)
;;

let%test_unit "snd" =
  Accessor_test_helpers.field
    (module Bool)
    (module Accessor_test_helpers.Testable.Tuple (Bool) (Bool))
    (module Unit)
    (fun () -> Accessor.Tuple2.snd)
;;

let%test_unit "swap" =
  Accessor_test_helpers.isomorphism
    (module Accessor_test_helpers.Testable.Tuple (Bool) (Bool))
    (module Accessor_test_helpers.Testable.Tuple (Bool) (Bool))
    (module Unit)
    (fun () -> Accessor.Tuple2.swap)
;;

let%test_unit "assocl" =
  Accessor_test_helpers.isomorphism
    (module Accessor_test_helpers.Testable.Tuple
              (Accessor_test_helpers.Testable.Tuple (Bool) (Bool)) (Bool))
    (module Accessor_test_helpers.Testable.Tuple
              (Bool)
              (Accessor_test_helpers.Testable.Tuple (Bool) (Bool)))
    (module Unit)
    (fun () -> Accessor.Tuple2.assocl)
;;

let%test_unit "assocr" =
  Accessor_test_helpers.isomorphism
    (module Accessor_test_helpers.Testable.Tuple
              (Bool)
              (Accessor_test_helpers.Testable.Tuple (Bool) (Bool)))
    (module Accessor_test_helpers.Testable.Tuple
              (Accessor_test_helpers.Testable.Tuple (Bool) (Bool)) (Bool))
    (module Unit)
    (fun () -> Accessor.Tuple2.assocr)
;;

let%test_unit "each" =
  Accessor_test_helpers.nonempty
    (module Bool)
    (module Accessor_test_helpers.Testable.Tuple (Bool) (Bool))
    (module Unit)
    (fun () -> Accessor.Tuple2.each)
;;
