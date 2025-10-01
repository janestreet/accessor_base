open! Base
open! Import

(** Access whether a key is present in the set. [true] means the key is present, and
    [false] means it is absent. *)
val mem : 'key -> (_, bool, ('key, _) Set.t, [< field ]) Accessor.t

val at : 'key -> (_, bool, ('key, _) Set.t, [< field ]) Accessor.t
[@@deprecated "[since 2020-09] Use [mem] instead of [at]"]

(** The indexed version of [mem] adds the given key to the index. *)
val memi : 'key -> ('key * 'i, 'i, bool, ('key, _) Set.t, [< field ]) Accessor.Indexed.t

val ati : 'key -> ('key * 'i, 'i, bool, ('key, _) Set.t, [< field ]) Accessor.Indexed.t
[@@deprecated "[since 2020-09] Use [memi] instead of [ati]"]

(** Access [()] iff the set contains the given key. *)
val found : 'key -> (_, unit, ('key, _) Set.t, [< optional ]) Accessor.t

(** The indexed version of [found] adds the given key to the index. *)
val foundi
  :  'key
  -> ('key * 'i, 'i, unit, ('key, _) Set.t, [< optional ]) Accessor.Indexed.t

(** Access every element in a set. *)
val each
  : ('i -> 'key -> _, 'i -> ('key, 'cmp) Set.t -> _, [< many_getter ]) Accessor.General.t

(** Treat [None] equivalently with the empty set. This accessor is not well-behaved, as it
    violates [construct (get at) = at]:

    [construct (get (Some Foo.Set.empty)) = construct Foo.Set.empty = None] *)
val empty_default
  :  ('k1, 'cmp1) Comparator.Module.t
  -> ( 'i -> ('k1, 'cmp1) Set.t -> ('k2, 'cmp2) Set.t
       , 'i -> ('k1, 'cmp1) Set.t option -> ('k2, 'cmp2) Set.t option
       , [< isomorphism ] )
       Accessor.General.t

(** [of_accessor (module M) accessor x] is a [M.Set.t] that contains everything accessed
    by [accessor] in [x]. *)
val of_accessor
  :  ('a, 'cmp) Comparator.Module.t
  -> (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.General.t
  -> 'at
  -> ('a, 'cmp) Set.t
