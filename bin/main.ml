open Forester_refs
open Core
module Terminal = Asai.Tty.Make (Reporter.Message)
open Dream_html
open HTML

let render_ctx (ctx : id_ctx) =
  span [] @@ List.map (fun s -> txt "%s" (fst s)) ctx

let render_label (l : int list) = div [] @@ List.map (fun i -> txt "%d." i) l

open Sem

let tree =
  let code = Text "asdf" in
  {
    title = None;
    addr = Some "";
    taxon = None;
    authors = [];
    tags = [];
    dates = [];
    metas = [];
    source_path = None;
    body = [ { Asai.Range.loc = None; value = code } ];
  }

open Load_forest

let () =
  let display d = Terminal.display d in
  Eio_main.run @@ fun env ->
  Reporter.run ~emit:display ~fatal:(fun d ->
      display d;
      exit 1)
  @@ fun () ->
  let forest = load_forest env "./forest" in
  let toc = section_ids_at_depth tree [ 1 ] forest in
  Dream.run ~port:1234 @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream_html.respond
             @@ div [] [ render_label @@ snd toc; render_ctx @@ fst toc ]);
       ]
