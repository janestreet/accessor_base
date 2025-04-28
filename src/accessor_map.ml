open! Base
open! Import

let ati key =
  Accessor.fieldi
    ~get:(fun t -> key, Map.find t key)
    ~set:(fun t data ->
      match data with
      | None -> Map.remove t key
      | Some data -> Map.set t ~key ~data)
;;

let at key = ati key @> Accessor.map_index Accessor.Index.tl
let found key = at key @> Accessor_option.some
let foundi key = ati key @> Accessor_option.some

let traverse (type a b) () =
  let module Map_traversal =
    Map.Make_applicative_traversals (struct
      include Accessor.Many

      type nonrec 'x t = ('x, a, b) t

      let of_thunk f = f ()
    end)
  in
  fun x -> Map_traversal.mapi x ~f:(fun ~key:_ ~data -> Accessor.Many.access data)
;;

let traversei (type k a b) () =
  let module Map_traversal =
    Map.Make_applicative_traversals (struct
      include Accessor.Many

      type nonrec 'x t = ('x, k * a, b) t

      let of_thunk f = f ()
    end)
  in
  fun x -> Map_traversal.mapi x ~f:(fun ~key ~data -> Accessor.Many.access (key, data))
;;

let each = [%accessor Accessor.many (traverse ())]
let eachi = [%accessor Accessor.manyi (traversei ())]

(* [subrange] is not well behaved on its own. It's used to define separate well-behaved
   accessors [each_in_subrange] and [each_in_subrangei]. *)
let subrange ~lower_bound ~upper_bound =
  Accessor.field
    ~get:(fun t -> Map.subrange t ~lower_bound ~upper_bound)
    ~set:(fun t subrange ->
      Map.merge_skewed t subrange ~combine:(fun ~key:_ _prev next -> next))
;;

let each_in_subrange ~lower_bound ~upper_bound =
  subrange ~lower_bound ~upper_bound @> each
;;

let each_in_subrangei ~lower_bound ~upper_bound =
  subrange ~lower_bound ~upper_bound @> eachi
;;

let empty_default comparator =
  Accessor_option.default (Map.empty comparator) ~is_default:Map.is_empty
;;

let of_accessor_aux comparator accessor at ~key_of_index ~of_alist =
  Accessor.to_listi accessor at
  |> List.map ~f:(fun (k, d) -> key_of_index k, d)
  |> of_alist comparator
;;

let of_accessor comparator accessor at ~key_of_index =
  of_accessor_aux comparator accessor at ~key_of_index ~of_alist:Map.of_alist
;;

let of_accessor_exn comparator accessor at ~key_of_index =
  of_accessor_aux comparator accessor at ~key_of_index ~of_alist:Map.of_alist_exn
;;

let of_accessor_fold comparator accessor at ~key_of_index ~init ~f =
  of_accessor_aux
    comparator
    accessor
    at
    ~key_of_index
    ~of_alist:(Map.of_alist_fold ~init ~f)
;;

let of_accessor_multi comparator accessor at ~key_of_index =
  of_accessor_aux comparator accessor at ~key_of_index ~of_alist:Map.of_alist_multi
;;

let of_accessor_or_error comparator accessor at ~key_of_index =
  of_accessor_aux comparator accessor at ~key_of_index ~of_alist:Map.of_alist_or_error
;;

let of_accessor_reduce comparator accessor at ~key_of_index ~f =
  of_accessor_aux comparator accessor at ~key_of_index ~of_alist:(Map.of_alist_reduce ~f)
;;

include Accessor.Of_applicative_without_return3 (struct
    type ('data, 'key, 'cmp) t = ('key, 'data, 'cmp) Map.t

    let map = Map.map

    let apply t1 t2 =
      if Map.length t1 <= Map.length t2
      then Map.filter_mapi t1 ~f:(fun ~key ~data:f -> Option.map (Map.find t2 key) ~f)
      else
        Map.filter_mapi t2 ~f:(fun ~key ~data:x ->
          Option.map (Map.find t1 key) ~f:(fun f -> f x))
    ;;
  end)
