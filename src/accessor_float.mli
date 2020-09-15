open! Base
open! Import

(** Access the negation of a float. *)
val negated : (_, float, float, [< isomorphism ]) Accessor.t

(** [add], [subtract], [multiply], and [divide] access modified versions of a float. They
    are not well-behaved accessors, because they do not necessarily round trip. *)

val added : float -> (_, float, float, [< isomorphism ]) Accessor.t
val subtracted : float -> (_, float, float, [< isomorphism ]) Accessor.t
val multiplied : float -> (_, float, float, [< isomorphism ]) Accessor.t
val divided : float -> (_, float, float, [< isomorphism ]) Accessor.t
