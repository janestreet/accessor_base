open! Base
open! Import

(** See also: [Accessor_core.List] *)

(** Access [()] iff the list is empty. *)
val nil : (_, unit, 'a list, [< variant ]) Accessor.t

(** Access the head and tail of a list, if it is nonempty. *)
val cons
  : ( 'i -> 'a * 'a list -> 'b * 'b list
    , 'i -> 'a list -> 'b list
    , [< variant ] )
    Accessor.General.t

(** Access an element at a specified position in a list, if the list is long enough to
    have such an element. *)
val nth : int -> (_, 'a, 'a list, [< optional ]) Accessor.t

(** Access a reversed version of a list. *)
val reversed
  : ( 'i -> 'a list -> 'b list
    , 'i -> 'a list -> 'b list
    , [< isomorphism ] )
    Accessor.General.t

(** [prefixed prefix ~equal] verifies that a list starts with [prefix], accessing the
    suffix left after stripping the prefix if so. *)
val prefixed
  :  'a list
  -> equal:('a -> 'a -> bool)
  -> (_, 'a list, 'a list, [< variant ]) Accessor.t

(** [suffixed suffix ~equal] verifies that a list ends with [suffix], accessing the prefix
    left after stripping the suffix if so. *)
val suffixed
  :  'a list
  -> equal:('a -> 'a -> bool)
  -> (_, 'a list, 'a list, [< variant ]) Accessor.t

(** Access every element in a list. *)
val each : ('i -> 'a -> 'b, 'i -> 'a list -> 'b list, [< many ]) Accessor.General.t

(** Like [each], but also provides you with the index of each element. *)
val eachi
  : (int * 'it -> 'a -> 'b, 'it -> 'a list -> 'b list, [< many ]) Accessor.General.t

include Accessor.Monad.S with type 'a t := 'a list
