open Forester_refs
open Core
module Terminal = Asai.Tty.Make (Reporter.Message)
open Dream_html
open HTML

let render_toc (toc : (Core.addr * int list) list) =
  div []
    (List.map
       (fun (addr, nums) ->
         div []
           [
             (*  NOTE: Fresh counters are added to the beginning of the list, so we reverse. *)
             span [] (List.map (fun d -> txt "%d. " d) (List.rev nums));
             txt "%s" addr;
           ])
       toc)

open Load_forest

let () =
  let display d = Terminal.display d in
  Eio_main.run @@ fun env ->
  Reporter.run ~emit:display ~fatal:(fun d ->
      display d;
      exit 1)
  @@ fun () ->
  let forest = load_forest env "./trees" in
  let toc = addrs_at_depth_list [ "index" ] [ 1 ] forest in
  Dream.run ~port:1234 @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream_html.respond @@ div [] [ render_toc toc ]);
       ]
