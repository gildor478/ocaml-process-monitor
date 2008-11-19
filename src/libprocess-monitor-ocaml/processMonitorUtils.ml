
(** Utilities for ocaml-process-monitor.
    @author Sylvain Le Gall
  *)

open ProcessMonitorTypes;;

module MapPid = Map.Make(
struct
  type t = pid
  let compare p1 p2 = p1 - p2
end)
;;

let map_pid_push pid v mp =
  let former =
    try 
      MapPid.find pid mp
    with Not_found ->
      []
  in
    MapPid.add pid (v :: former) mp
;;

let string_of_bytes b =
  let kb_limit =
    1024.0
  in
  let mb_limit =
    1024.0 *. kb_limit
  in
  let gb_limit =
    1024.0 *. mb_limit
  in
  let printf_with_unit div unt =
    Printf.sprintf "%.2f %s" (b /. div) unt
  in
    if b < kb_limit then
      printf_with_unit 1.0 "B"
    else if b < mb_limit then
      printf_with_unit kb_limit "KB"
    else if b < gb_limit then
      printf_with_unit mb_limit "MB"
    else 
      printf_with_unit gb_limit "GB"
;;

