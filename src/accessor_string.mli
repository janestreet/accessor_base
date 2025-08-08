open! Base
open! Import

(** Access the characters in a string as a list. *)
val list : (_, char list, string, [< isomorphism ]) Accessor.t

(** Access a reversed version of a string. *)
val reversed : (_, string, string, [< isomorphism ]) Accessor.t

(** Access each character in a string. *)
val each : (_, char, string, [< many ]) Accessor.t

(** The indexed version of [each] adds the numeric index of each character accessed to the
    index. *)
val eachi : (int * 'i, 'i, char, string, [< many ]) Accessor.Indexed.t

(** Access the suffix of a string that begins with the given prefix, or nothing if the
    string has a different prefix. *)
val prefixed : string -> (_, string, string, [< variant ]) Accessor.t

(** Access the prefix of a string that ends with the given suffix, or nothing if the
    string has a different suffix. *)
val suffixed : string -> (_, string, string, [< variant ]) Accessor.t

(** Access the string converted to the given type, or access nothing if conversion fails.
    The well behavedness of this accessor just depends on the [Stringable] argument. *)
val conv
  :  (module Stringable.S with type t = 'a)
  -> (_, 'a, string, [< variant ]) Accessor.t

(** Access the string converted to the given type, raising if conversion fails. The well
    behavedness of this accessor just depends on the [Stringable] argument. *)
val conv_strict
  :  (module Stringable.S with type t = 'a)
  -> (_, 'a, string, [< isomorphism ]) Accessor.t
