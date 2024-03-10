open Core
open Sem
open Forester
open Forest
module A = Analysis
module M = A.Map
module Gph = A.Gph
module L = Lsp.Types

(*
  NOTE: A polymorphic numbering system will allow us to create seperate counters
  for figures, references, authors, etc...
*)

(*
   TODO: incorporate lsp types, generate diagnostics from invalid addrs.
   Which messages would be useful here?
*)

type id_ctx = (addr * int list) list

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

and children addr forest =
  let a = Lazy.force forest.analysis in
  let graph = a.transclusion_graph in
  (*
    FIXME: The predecessors in the transclusion graph are not in the correct
    order. We have to scan tree.body instead. Reversing them happens to give me
    the correct output here, but I need to check if this is always the case
  *)
  Gph.pred graph addr |> List.rev |> fun a -> lookup_each forest a

and lookup_each forest addrs =
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

and section_ids n forest : id_ctx =
  let ctx, _ = addrs_at_depth n [ 1 ] forest in
  ctx

and valid tree forest =
  tree.body
  |> List.filter_map (fun n ->
         match n with
         | Range.{ value = Transclude (_, addr); _ } -> Some addr
         | _ -> None)
  |> List.for_all (fun addr -> M.find_opt addr forest.trees |> Option.is_some)

and lsp_pos_of_pos (pos : Asai.Range.position) =
  L.Position.create ~line:(pos.line_num - 1)
    ~character:(pos.offset - pos.start_of_line)

and lsp_range_of_range (r : Asai.Range.t option) =
  match r with
  | Some r ->
      let start, stop =
        match Asai.Range.view r with
        | `Range (start, stop) -> (start, stop)
        | `End_of_file pos -> (pos, pos)
      in
      L.Range.create ~start:(lsp_pos_of_pos start) ~end_:(lsp_pos_of_pos stop)
  | None ->
      (* When we have a message without a location,
         we set it's location to the start of the file,
         as we don't have any better choices. *)
      let start_of_file = L.Position.create ~line:0 ~character:0 in
      L.Range.create ~start:start_of_file ~end_:start_of_file

and invalid tree forest =
  tree.body
  |> List.filter_map (fun n ->
         match n with
         | Range.{ value = Transclude (_, addr); loc } -> (
             match M.find_opt addr forest.trees with
             | None -> (
                 match loc with
                 | Some l -> Some (to_diagnostic addr (Some l))
                 | _ -> None)
             | Some _ -> None)
         | _ -> None)

and to_diagnostic addr loc : Lsp.Types.Diagnostic.t =
  {
    code = None;
    codeDescription = None;
    data = None;
    message = Format.sprintf "Invalid address `%s`" addr;
    range = lsp_range_of_range loc;
    relatedInformation = Some [];
    severity = Some Error;
    source = None;
    tags = None;
  }

let 

let rec reforest (ns : Sem.t) (par : Sem.t) : Sem.t =
  let flush_par = Range.{ loc = None; value = Prim (`P, List.rev par) } in
  match ns with
  | [] -> [ flush_par ]
  | n :: ns' -> (
      match n.value with Math (_, _) -> [] | _ -> flush_par :: reforest ns' [])
