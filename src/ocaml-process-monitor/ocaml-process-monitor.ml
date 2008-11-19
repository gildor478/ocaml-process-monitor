
(** Watch ressource of a process
    @author Sylvain Le Gall
  *)

open ProcessMonitor;;
open ProcessMonitorUtils;;
open ProcessMonitorWatchData;;

let () = 
  let rinterval =
    ref 1.0
  in
  let rnoinplace =
    ref false
  in
  let rconf =
    ref 
      {
        prog            = "";
        args            = [||];
        watch_resources = false;
        watch_children  = false;
        watch_dir       = [];
        limit_mem       = None;
        limit_time      = None;
      }
  in
  let args = 
    [
      "--interval",
      Arg.Set_float rinterval,
      "seconds Interval to check process status";

      "--no-inplace",
      Arg.Set rnoinplace,
      " Don't write status line in-place";

      "--watch-resources",
      Arg.Unit 
        (fun () ->
           rconf := {!rconf with watch_resources = true}),
      " Watch resources used by the process";

      "--watch-children",
      Arg.Unit
        (fun () ->
           rconf := {!rconf with watch_children = true}),
      " Watch resources used by the process";

      "--watch-dir",
      Arg.String
        (fun str ->
           rconf := {!rconf with watch_dir = str :: !rconf.watch_dir}),
      "dir Watch directory size";

      "--",
      Arg.Rest 
        (fun str -> 
           rconf := {!rconf with args = Array.append !rconf.args [|str|]}),
      "[...] Process arguments";
    ]
  in

  let () =
    Arg.parse 
      (Arg.align args)
      (fun str -> 
         if !rconf.prog <> "" then
           Printf.eprintf "Overriding former program name %S" !rconf.prog;

         rconf := {!rconf with prog = str})
      ProcessMonitorConfig.copyright
  in

  let t = 
    create !rconf
  in

  let print_memory (last_width, max_size) wd = 
    (* Begin to erase last data printed *)
    let erase_str =
      String.concat ""
        [
          String.make last_width '\b';
          String.make last_width ' ';
          String.make last_width '\b';
        ]
    in
    let max_size = 
      max max_size wd.vmsize
    in
    let new_str = 
      Printf.sprintf 
        "Time: %.2fs; VMSize: %s; VMRss: %s; Max. size: %s; Process: %d; Directory size: %s;"
        wd.timestamp
        (string_of_bytes wd.vmsize)
        (string_of_bytes wd.vmrss)
        (string_of_bytes max_size)
        (MapPid.fold (fun _ _ i -> i + 1) wd.state 0)
        wd.dirsize
    in
    let new_width =
      if !rnoinplace then
        0
      else
        String.length new_str
    in
      Printf.printf "%s%s%!" erase_str new_str;
      if !rnoinplace then
        print_newline ();
      new_width, max_size
  in

  let process_status =
    auto_poll 
      t 
      !rinterval 
      print_memory
      (0, 0.0)
  in

    print_newline ();

    match process_status with 
      | Unix.WEXITED i ->
          Printf.printf "Exit code %d\n%!" i;
          exit i
      | Unix.WSIGNALED i ->
          Printf.eprintf "Killed by signal %d\n%!" i;
          exit 1
      | Unix.WSTOPPED i ->
          Printf.eprintf "Stopped by signal %d\n%!" i;
          exit 1

;;

