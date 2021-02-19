open! Core
open! Import

type t =
  { foo : int * int
  ; bar : int * int
  }
[@@deriving compare, quickcheck, sexp_of]

let each =
  [%accessor
    Accessor.nonempty (fun { foo; bar } ->
      let%map_open.Accessor.Nonempty foo = access_nonempty Accessor.Tuple2.each foo
      and bar = access_nonempty Accessor.Tuple2.each bar in
      { foo; bar })]
;;

let%expect_test _ =
  Quickcheck.test [%quickcheck.generator: t * (int -> int)] ~f:(fun (t, f) ->
    let observed = Accessor.map each t ~f in
    let expect =
      let { foo = foo1, foo2; bar = bar1, bar2 } = t in
      { foo = f foo1, f foo2; bar = f bar1, f bar2 }
    in
    [%test_result: t] observed ~expect)
;;
