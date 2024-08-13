open! Base
open! Import

(** Access the first component of a tuple. *)
val fst : ('i -> 'a -> 'b, 'i -> 'a * 'c -> 'b * 'c, [< field ]) Accessor.General.t

(** Access the second component of a tuple. *)
val snd : ('i -> 'a -> 'b, 'i -> 'c * 'a -> 'c * 'b, [< field ]) Accessor.General.t

(** [fsti] and [sndi] are like [fst] and [snd], but they also treat the untargetted
    component as an index. For example, if you use [sndi] on [x, y], [y] is what you are
    accessing, and [x] will be given to you as an index. *)

val fsti
  : ('i * 'it -> 'a -> 'b, 'it -> 'a * 'i -> 'b * 'i, [< field ]) Accessor.General.t

val sndi
  : ('i * 'it -> 'a -> 'b, 'it -> 'i * 'a -> 'i * 'b, [< field ]) Accessor.General.t

(** Access a tuple where the components are swapped with each other. *)
val swap
  : ( 'i -> 'a * 'b -> 'c * 'd
      , 'i -> 'b * 'a -> 'd * 'c
      , [< isomorphism ] )
      Accessor.General.t

(** [assocl] and [assocr] reassociate nested tuples. *)

val assocl
  : ( 'i -> ('a * 'b) * 'c -> ('d * 'e) * 'f
      , 'i -> 'a * ('b * 'c) -> 'd * ('e * 'f)
      , [< isomorphism ] )
      Accessor.General.t

val assocr
  : ( 'i -> 'a * ('b * 'c) -> 'd * ('e * 'f)
      , 'i -> ('a * 'b) * 'c -> ('d * 'e) * 'f
      , [< isomorphism ] )
      Accessor.General.t

(** Access both components of a tuple. *)
val each : ('i -> 'a -> 'b, 'i -> 'a * 'a -> 'b * 'b, [< nonempty ]) Accessor.General.t

module Fst : Accessor.Functor.S2 with type ('fst, 'snd) t := 'fst * 'snd
module Snd : Accessor.Functor.S2 with type ('snd, 'fst) t := 'fst * 'snd
