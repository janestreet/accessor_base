open! Core_kernel
open! Import

let%expect_test "no stack overflow on List.each" =
  let xs = List.init 10_000_000 ~f:Fn.id in
  Expect_test_helpers_kernel.require_does_not_raise ~cr:CR_soon [%here] (fun () ->
    Accessor.iter Accessor.List.each xs ~f:(fun (_ : int) -> ()));
  [%expect
    {|
    ("unexpectedly raised" ("Stack overflow")) |}]
;;
