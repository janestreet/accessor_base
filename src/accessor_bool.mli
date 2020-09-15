open! Base
open! Import

(** Accesses [()] iff the boolean is [true]. *)
val true_ : (_, unit, bool, [< variant ]) Accessor.t

(** Accesses [()] iff the boolean is [false]. *)
val false_ : (_, unit, bool, [< variant ]) Accessor.t

(** Access a boolean as its inverse. *)
val negated : (_, bool, bool, [< isomorphism ]) Accessor.t
