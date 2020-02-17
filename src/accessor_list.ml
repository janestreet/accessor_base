open! Base
open! Import

let reversed = [%accessor Accessor.isomorphism ~get:List.rev ~construct:List.rev]

(* The reasoning for naming the following functions as [nil] and [cons] is that if [list]
   was defined as a normal ADT, it would probably be:

   {[
     type 'a t =
       | Nil
       | Cons of 'a * 'a t
   ]}

   The convention for [variant] accessors is to name them the same way we would name
   native OCaml constructors. *)

let nil =
  [%accessor
    Accessor.variant
      ~match_:(function
        | [] -> First ()
        | _ :: _ as list -> Second list)
      ~construct:(fun () -> [])]
;;

let cons =
  [%accessor
    Accessor.variant
      ~match_:(function
        | [] -> Second []
        | x :: xs -> First (x, xs))
      ~construct:(fun (x, xs) -> x :: xs)]
;;

let split_n i =
  Accessor.isomorphism
    ~get:(fun xs -> List.split_n xs i)
    ~construct:(fun (prefix, suffix) -> prefix @ suffix)
;;

let nth i = split_n i @> Accessor_tuple2.snd @> cons @> Accessor_tuple2.fst

let prefixed prefix ~equal =
  [%accessor
    Accessor.variant
      ~match_:(fun xs ->
        let p, s = List.split_n xs (List.length prefix) in
        if List.equal equal prefix p then First s else Second xs)
      ~construct:(fun s -> prefix @ s)]
;;

let suffixed suffix ~equal = reversed @> prefixed (List.rev suffix) ~equal @> reversed

let each =
  [%accessor
    Accessor.many (fun at -> Accessor.Many.all (List.map at ~f:Accessor.Many.access))]
;;

let eachi =
  [%accessor
    Accessor.manyi (fun at ->
      Accessor.Many.all (List.mapi at ~f:(fun i a -> Accessor.Many.access (i, a))))]
;;

include Accessor.Of_monad (struct
    include List

    let apply = `Define_using_bind
  end)
