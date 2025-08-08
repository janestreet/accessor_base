open! Core
open! Import
module Bool_option = Accessor_test_helpers.Testable.Option (Bool)

module Bool_bool_map = struct
  include Accessor_test_helpers.Testable.Bool_map (Bool)

  module Nonempty = struct
    type nonrec t = t [@@deriving equal, quickcheck, sexp_of]

    let quickcheck_generator =
      Quickcheck.Generator.filter quickcheck_generator ~f:(Fn.non Map.is_empty)
    ;;

    let quickcheck_shrinker =
      Quickcheck.Shrinker.filter quickcheck_shrinker ~f:(Fn.non Map.is_empty)
    ;;
  end
end

module Bool_maybe_bound = struct
  open struct
    module Maybe_bound = struct
      type 'a t = 'a Maybe_bound.t =
        | Incl of 'a
        | Excl of 'a
        | Unbounded
      [@@deriving equal, quickcheck, sexp_of]
    end
  end

  type t = bool Maybe_bound.t [@@deriving equal, quickcheck, sexp_of]
end

let%test_unit "at" =
  Accessor_test_helpers.field
    (module Bool_option)
    (module Bool_bool_map)
    (module Bool)
    Accessor.Map.at
;;

let%test_unit "found" =
  Accessor_test_helpers.optional
    (module Bool)
    (module Bool_bool_map)
    (module Bool)
    Accessor.Map.found
;;

let%test_unit "each" =
  Accessor_test_helpers.many (module Bool) (module Bool_bool_map) (module Unit) (fun () ->
    Accessor.Map.each)
;;

let%test_unit "each_in_subrange" =
  Accessor_test_helpers.many
    (module Bool)
    (module Bool_bool_map)
    (module Accessor_test_helpers.Testable.Tuple (Bool_maybe_bound) (Bool_maybe_bound))
    (fun (lower_bound, upper_bound) ->
       Accessor.Map.each_in_subrange ~lower_bound ~upper_bound)
;;

(* [empty_default] is known and documented to not be well behaved. *)
let%expect_test "empty_default is not always well behaved" =
  Expect_test_helpers_core.require_does_raise ~hide_positions:true (fun () ->
    Accessor_test_helpers.isomorphism
      (module Bool_bool_map)
      (module Accessor_test_helpers.Testable.Option (Bool_bool_map))
      (module Unit)
      (fun () -> Accessor.Map.empty_default (module Bool)));
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

let%test_unit "empty_default is well behaved as long as you don't use [Some Map.empty]" =
  Accessor_test_helpers.isomorphism
    (module Bool_bool_map)
    (module Accessor_test_helpers.Testable.Option (Bool_bool_map.Nonempty))
    (module Unit)
    (fun () -> Accessor.Map.empty_default (module Bool))
;;
