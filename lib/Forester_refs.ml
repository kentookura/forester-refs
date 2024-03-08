open Core
open Sem
open Forester
open Forest
module A = Analysis
module M = A.Map
module Gph = A.Gph

type id_ctx = (addr * int list) list

let children addr forest =
  let a = Lazy.force forest.analysis in
  let graph = a.transclusion_graph in
  Gph.pred graph addr

let rec section_ids_at_depth (tree : tree) (d : int list) forest :
    id_ctx * int list =
  let k, ks =
    match d with
    | [] -> failwith "section_ids_at_depth: empty depth"
    | k :: ks -> (k, ks)
  in
  let _foo (n : node Range.located) =
    match n.value with
    | Text _ -> ([], k :: ks)
    | Transclude (_opts, addr) ->
        (* let ctx = *)
        let _ =
          section_ids_at_depth_list (children addr forest) (1 :: k :: ks) forest
        in
        ([], [])
    | _ -> ([], k :: ks)
  in
  let _ = List.map _foo tree.body in
  ([], k :: ks)

and lookup_each addrs forest =
  let rec step ((known, queue) : tree list * addr list) =
    match queue with
    | [] -> known
    | x :: xs -> (
        match M.find_opt x forest.trees with
        | None -> step (known, xs)
        | Some tree -> step (tree :: known, xs))
  in
  step ([], addrs)

and section_ids_at_depth_list (children : _ list) d forest =
  let ctx, _ =
    List.fold_left
      (fun (ctx, d) n' ->
        let ctx', d' = section_ids_at_depth n' d forest in
        (ctx @ ctx', d'))
      ([], d)
      (lookup_each children forest)
  in
  ctx

(* section_ids_at_depth n [1] in ctx *)
