open Core
open Sem
open Forester
open Forest
module A = Analysis
module M = A.Map
module Gph = A.Gph

(*
  NOTE: For fully evaluated (static) forests, you would only use
  import_graph to create a table of contents. However, when we are editing a
  tree or dynamically render fragments, it might be useful to be able to
  create a table of contents from a Sem.tree. The problem I'm facing is that
  we can't just do a simple fold as in the paper. When we create the table of
  contents of an open tree, we assume the forest being referenced by a
  transclusion to be static.
*)

(*
  NOTE: A polymorphic numbering system will allow us to create seperate counters
  for figures, references, authors, etc...
*)

(*
   TODO: incorporate lsp types, generate diagnostics from invalid addrs.
   Which messages would be useful here?
*)

type id_ctx = (addr * int list) list

let lookup_each forest addrs =
  let rec step (acc, queue) =
    match queue with
    | [] -> acc
    | x :: xs -> (
        match M.find_opt x forest.trees with
        | None -> step (acc, xs)
        | Some tree -> (
            match tree.addr with
            | Some addr -> step (addr :: acc, xs)
            | None -> step (acc, xs)))
  in
  step ([], addrs)

let children addr forest =
  let a = Lazy.force forest.analysis in
  let graph = a.transclusion_graph in
  (*
    FIXME: The predecessors in the transclusion graph are not in the correct
    order.
  *)
  Gph.pred graph addr |> fun a -> lookup_each forest a

let rec addrs_at_depth addr (d : int list) forest : id_ctx * int list =
  let k, ks =
    match d with
    | [] -> failwith "section_ids_at_depth: empty depth"
    | k :: ks -> (k, ks)
  in
  let children = children addr forest in
  match M.find_opt addr forest.trees with
  | Some _ ->
      let ctx = addrs_at_depth_list children (1 :: k :: ks) forest in
      ((addr, k :: ks) :: ctx, (k + 1) :: ks)
  | None ->
      let ctx = addrs_at_depth_list children (k :: ks) forest in
      (ctx, k :: ks)

and addrs_at_depth_list children d forest =
  let ctx, _ =
    List.fold_left
      (fun (ctx, d) n' ->
        let ctx', d' = addrs_at_depth n' d forest in
        (ctx @ ctx', d'))
      ([], d) children
  in
  ctx

let section_ids n forest : id_ctx =
  let ctx, _ = addrs_at_depth n [ 1 ] forest in
  ctx

and valid tree forest =
  tree.body
  |> List.filter_map (fun n ->
         match n with
         | Range.{ value = Transclude (_, addr); _ } ->
             M.find_opt addr forest.trees
         | _ -> None)

and invalid tree forest =
  tree.body
  |> List.filter_map (fun n ->
         match n with
         | Range.{ value = Transclude (_, addr); loc } -> (
             match M.find_opt addr forest.trees with
             | None ->
                 let diagnostic = loc in
                 Some diagnostic
             | Some _ -> None)
         | _ -> None)
