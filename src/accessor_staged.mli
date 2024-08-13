open! Base
open! Import

val staged
  : ( 'i -> 'a Staged.t -> 'b Staged.t
      , 'i -> 'a -> 'b
      , [< isomorphism ] )
      Accessor.General.t

val unstaged
  : ( 'i -> 'a -> 'b
      , 'i -> 'a Staged.t -> 'b Staged.t
      , [< isomorphism ] )
      Accessor.General.t
