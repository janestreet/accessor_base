open! Core
open! Import
module Bool_list = Accessor_test_helpers.Testable.List (Bool)

let%test_unit "nil" =
  Accessor_test_helpers.variant (module Unit) (module Bool_list) (module Unit) (fun () ->
    Accessor.List.nil)
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
  Accessor_test_helpers.many (module Bool) (module Bool_list) (module Unit) (fun () ->
    Accessor.List.each)
;;

let%expect_test "monadic fold" =
  let module M = struct
    module T0 = struct
      type 'a t =
        | Return of 'a
        | Effect of bool * 'a t
      [@@deriving compare, sexp_of, quickcheck]

      let return a = Return a
      let map = `Define_using_bind

      let rec bind t ~f =
        match t with
        | Return a -> f a
        | Effect (n, t) -> Effect (n, bind t ~f)
      ;;
    end

    module T1 = struct
      include T0
      include Monad.Make (T0)

      let apply = `Define_using_bind
    end

    include T1
    include Accessor.Of_monad (T1)
  end
  in
  Quickcheck.test
    [%quickcheck.generator: bool list * bool * (bool -> bool -> bool M.t)]
    ~f:(fun (xs, init, f) ->
      [%test_result: bool M.t]
        (M.fold Accessor.List.each xs ~init ~f)
        ~expect:
          (List.fold xs ~init:(M.return init) ~f:(fun acc a ->
             M.bind acc ~f:(fun acc -> f acc a))))
;;
