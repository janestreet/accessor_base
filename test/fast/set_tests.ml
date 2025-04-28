open! Core
open! Import

module Bool_set = struct
  include Accessor_test_helpers.Testable.Bool_set

  module Nonempty = struct
    type nonrec t = t [@@deriving equal, quickcheck, sexp_of]

    let quickcheck_generator =
      Quickcheck.Generator.filter quickcheck_generator ~f:(Fn.non Set.is_empty)
    ;;

    let quickcheck_shrinker =
      Quickcheck.Shrinker.filter quickcheck_shrinker ~f:(Fn.non Set.is_empty)
    ;;
  end
end

let%test_unit "mem" =
  Accessor_test_helpers.field
    (module Bool)
    (module Bool_set)
    (module Bool)
    Accessor.Set.mem
;;

let%test_unit "found" =
  Accessor_test_helpers.optional
    (module Unit)
    (module Bool_set)
    (module Bool)
    Accessor.Set.found
;;

(* [empty_default] is known and documented to not be well behaved. *)
let%expect_test "empty_default is not always well behaved" =
  Expect_test_helpers_core.require_does_raise ~hide_positions:true (fun () ->
    Accessor_test_helpers.isomorphism
      (module Bool_set)
      (module Accessor_test_helpers.Testable.Option (Bool_set))
      (module Unit)
      (fun () -> Accessor.Set.empty_default (module Bool)));
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
      (input (() (())))
      (error (
        runtime.ml.E "comparison failed" (
          ()
          vs
          (())
          (Loc lib/accessor/test_helpers/accessor_test_helpers.ml:LINE:COL)))))
    |}]
;;

let%test_unit "empty_default is well behaved as long as you don't use [Some Set.empty]" =
  Accessor_test_helpers.isomorphism
    (module Bool_set)
    (module Accessor_test_helpers.Testable.Option (Bool_set.Nonempty))
    (module Unit)
    (fun () -> Accessor.Set.empty_default (module Bool))
;;
