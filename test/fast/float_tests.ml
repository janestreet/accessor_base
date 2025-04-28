open! Core
open! Import

module Float = struct
  include Float

  let equal x y = Int.equal (compare x y) 0
end

let isomorphism env accessor =
  Accessor_test_helpers.isomorphism (module Float) (module Float) env accessor
;;

let%test_unit "negated" = isomorphism (module Unit) (fun () -> Accessor.Float.negated)

(* The following test cases are expected to raise because [added], [subtracted],
   [multiplied], and [divided] are not well behaved in general. *)

let%expect_test "added" =
  Expect_test_helpers_core.require_does_raise ~hide_positions:true (fun () ->
    isomorphism (module Float) Accessor.Float.added);
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
      (input (-17.138797521591187 -3.950862943457765E-284))
      (error (
        runtime.ml.E "comparison failed" (
          0 vs -3.950862943457765E-284 (
            Loc lib/accessor/test_helpers/accessor_test_helpers.ml:LINE:COL)))))
    |}]
;;

let%expect_test "subtracted" =
  Expect_test_helpers_core.require_does_raise ~hide_positions:true (fun () ->
    isomorphism (module Float) Accessor.Float.subtracted);
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
      (input (-17.138797521591187 -3.950862943457765E-284))
      (error (
        runtime.ml.E "comparison failed" (
          0 vs -3.950862943457765E-284 (
            Loc lib/accessor/test_helpers/accessor_test_helpers.ml:LINE:COL)))))
    |}]
;;

let%expect_test "multiplied" =
  Expect_test_helpers_core.require_does_raise ~hide_positions:true (fun () ->
    isomorphism (module Float) Accessor.Float.multiplied);
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
      (input (-16479.23770776391 -1.43279037293961E-322))
      (error (
        runtime.ml.E "comparison failed" (
          -0 vs -1.43279037293961E-322 (
            Loc lib/accessor/test_helpers/accessor_test_helpers.ml:LINE:COL)))))
    |}]
;;

let%expect_test "divided" =
  Expect_test_helpers_core.require_does_raise ~hide_positions:true (fun () ->
    isomorphism (module Float) Accessor.Float.divided);
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
      (input (0.031249999999772626 1.96574886642148E-318))
      (error (
        runtime.ml.E "comparison failed" (
          1.96566981591815E-318
          vs
          1.96574886642148E-318
          (Loc lib/accessor/test_helpers/accessor_test_helpers.ml:LINE:COL)))))
    |}]
;;
