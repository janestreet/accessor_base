open! Core
open! Import

let%test_unit "negated" =
  Accessor_test_helpers.isomorphism (module Int) (module Int) (module Unit) (fun () ->
    Accessor.Int.negated)
;;

let%test_unit "added" =
  Accessor_test_helpers.isomorphism
    (module Int)
    (module Int)
    (module Int)
    Accessor.Int.added
;;

let%test_unit "substracted" =
  Accessor_test_helpers.isomorphism
    (module Int)
    (module Int)
    (module Int)
    Accessor.Int.subtracted
;;

let%test_unit "incremented" =
  Accessor_test_helpers.isomorphism (module Int) (module Int) (module Unit) (fun () ->
    Accessor.Int.incremented)
;;

let%test_unit "decremented" =
  Accessor_test_helpers.isomorphism (module Int) (module Int) (module Unit) (fun () ->
    Accessor.Int.decremented)
;;

let%test_unit "bit_negated" =
  Accessor_test_helpers.isomorphism (module Int) (module Int) (module Unit) (fun () ->
    Accessor.Int.bit_negated)
;;

let%test_unit "bit_xored" =
  Accessor_test_helpers.isomorphism
    (module Int)
    (module Int)
    (module Int)
    Accessor.Int.bit_xored
;;

let%test_unit "bit_at_exn" =
  let module Bit_index = struct
    type t = int [@@deriving quickcheck, sexp_of]

    let quickcheck_generator = Int.gen_uniform_incl 0 (Int.num_bits - 1)
  end
  in
  Accessor_test_helpers.field
    (module Bool)
    (module Int)
    (module Bit_index)
    Accessor.Int.bit_at_exn;
  Quickcheck.test
    (Quickcheck.Generator.union
       [ Int.gen_uniform_incl Int.min_value (-1)
       ; Int.gen_uniform_incl Int.num_bits Int.max_value
       ])
    ~f:(fun i ->
      match Accessor.Int.bit_at_exn i with
      | _ -> raise_s [%message "[Accessor.Int.bit_at_exn] should have raised" (i : int)]
      | exception _ -> ())
;;
