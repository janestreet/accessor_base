open! Core
open! Import
module Testable_either = Accessor_test_helpers.Testable.Either

let%test_unit "swapped" =
  Accessor_test_helpers.isomorphism
    (module Testable_either (Bool) (Bool))
    (module Testable_either (Bool) (Bool))
    (module Unit)
    (fun () -> Accessor.Either.swapped)
;;

let%test_unit "assocl" =
  Accessor_test_helpers.isomorphism
    (module Testable_either (Testable_either (Bool) (Bool)) (Bool))
    (module Testable_either (Bool) (Testable_either (Bool) (Bool)))
    (module Unit)
    (fun () -> Accessor.Either.assocl)
;;

let%test_unit "assocr" =
  Accessor_test_helpers.isomorphism
    (module Testable_either (Bool) (Testable_either (Bool) (Bool)))
    (module Testable_either (Testable_either (Bool) (Bool)) (Bool))
    (module Unit)
    (fun () -> Accessor.Either.assocr)
;;

let%test_unit "each" =
  Accessor_test_helpers.field
    (module Bool)
    (module Testable_either (Bool) (Bool))
    (module Unit)
    (fun () -> Accessor.Either.each)
;;
