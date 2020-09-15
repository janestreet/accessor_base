open! Base
open! Import

let memi key =
  Accessor.fieldi
    ~get:(fun t -> key, Set.mem t key)
    ~set:(fun t is_mem -> if is_mem then Set.add t key else Set.remove t key)
;;

let mem key = memi key @> Accessor.map_index Accessor.Index.tl
let foundi key = memi key @> Accessor_bool.true_
let found key = foundi key @> Accessor.map_index Accessor.Index.tl
let each = [%accessor Accessor.getter Set.to_list @> Accessor_list.each]

let empty_default comparator =
  Accessor_option.default (Set.empty comparator) ~is_default:Set.is_empty
;;

let of_accessor comparator accessor at =
  Set.of_list comparator (Accessor.to_list accessor at)
;;

let ati = memi
let at = mem
