open! Core
open! Import

let%bench_fun ("Accessor.iter Map.each" [@indexed len = [ 1; 10; 1000; 10000 ]]) =
  let map = Int.Map.of_alist_exn (List.init len ~f:(fun x -> x, x)) in
  fun () -> Accessor.iter Accessor.Map.each map ~f:ignore
;;

let%bench_fun ("Accessor.map Map.each" [@indexed len = [ 1; 10; 1000; 10000 ]]) =
  let map = Int.Map.of_alist_exn (List.init len ~f:(fun x -> x, x)) in
  fun () -> Accessor.map Accessor.Map.each map ~f:ignore
;;

module Identity = struct
  module M = struct
    type 'a t = 'a

    let return x = x
    let apply f x = f x
    let map = `Custom (fun x ~f -> f x)
  end

  include M
  include Applicative.Make (M)

  let of_thunk f = f ()
end

module Map_traversal = Map.Make_applicative_traversals (Identity)

let%bench_fun ("Map mapi baseline" [@indexed len = [ 1; 10; 1000; 10000 ]]) =
  let map = Int.Map.of_alist_exn (List.init len ~f:(fun x -> x, x)) in
  fun () -> Map_traversal.mapi map ~f:(fun ~key:_ ~data:_ -> ())
;;
