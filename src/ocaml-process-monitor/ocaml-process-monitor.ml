
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
  let rinplace =
    ref false
  in
  let default_process =  
    SubProcess("", [||])
  in
  let rconf =
    ref 
      {
        process         = default_process;
        watch_resources = true;
        watch_children  = true;
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

      "--inplace",
      Arg.Set rinplace,
      " Don't write status line in-place";

      "--nowatch-resources",
      Arg.Unit 
        (fun () ->
           rconf := {!rconf with watch_resources = false}),
      " Watch resources used by the process";

      "--nowatch-children",
      Arg.Unit
        (fun () ->
           rconf := {!rconf with watch_children = false}),
      " Watch resources used by the process";

      "--watch-dir",
      Arg.String
        (fun str ->
           rconf := {!rconf with watch_dir = str :: !rconf.watch_dir}),
      "dir Watch directory size";

      "--pid",
      Arg.Int
        (fun i ->
           rconf := {!rconf with process = Pid i}),
      "pid Watch process associated to pid";

      "--",
      Arg.Rest 
        (fun str -> 
           let process = 
             match !rconf.process with 
               | SubProcess (prog, args) ->
                   SubProcess (prog, Array.append args [|str|])
               | Pid pid ->
                   failwith 
                     (Printf.sprintf
                        "Watching process pid %d, cannot use process argument"
                        pid)
           in
             rconf := {!rconf with process = process}),
      "[...] Process arguments";
    ]
  in

  let () =
    Arg.parse 
      (Arg.align args)

      (fun str -> 
         let process = 
           match !rconf.process with 
             | SubProcess (prog, args) ->
                 if prog <> "" then
                   Printf.eprintf "Overriding former program name %S" prog;
                 SubProcess (str, args)
             | Pid pid ->
                 failwith 
                   (Printf.sprintf
                      "Watching process pid %d, cannot update program name"
                      pid)
         in
           rconf := {!rconf with process = process})

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
        "Time: %.2fs; Size: %s; Rss: %s; Max. size: %s; Proc. : %d; %s"
        wd.timestamp
        (string_of_bytes wd.vmsize)
        (string_of_bytes wd.vmrss)
        (string_of_bytes max_size)
        (MapPid.fold (fun _ _ i -> i + 1) wd.state 0)
        (String.concat "; "
           (List.map 
              (fun (dir, sz) ->
                 let fn_lst =
                   List.assoc dir wd.dirfiles
                 in
                   Printf.sprintf 
                     "Dir. %s: %s/%d"
                     dir
                     sz
                     (List.length fn_lst))
              wd.dirsize))
    in
    let new_width =
      if !rinplace then
        String.length new_str
      else
        0
    in
      Printf.printf "%s%s%!" erase_str new_str;
      if not !rinplace then
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

