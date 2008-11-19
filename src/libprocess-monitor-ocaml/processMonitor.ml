
(** Main interface of ocaml-process-monitor.
    @author Sylvain Le Gall
  *)

open ProcessMonitorTypes;;
open ProcessMonitorUtils;;
open ProcessMonitorConfig;;
open ProcessMonitorWatchData;;

module PML = ProcessMonitorLinux;;

(** {1 Types and exceptions}
  *)

(** Status return by poll.
  *)
type poll_result =
  | ProcessExited of Unix.process_status
  | ProcessLive of watch_data
;;

(** Configuration of monitoring 
  *)
type configuration =
  {
    (** [prog] and [args] used to run the process to monitor
      * see {!Unix.execv} and {!Unix.create_process}.
      *)
    prog:            string;
    args:            string array;

    (** Define what to watch for see {!watch_data}.
      *)
    watch_resources: bool;
    watch_children:  bool;
    watch_dir:       dirname list;

    (** Define a limit on execution time and memory used. It use [setrlimit]
      * and [Unix.fork] so it can be restricted to specific OS.
      *)
    limit_mem:       int option;
    limit_time:      int option;
  }
;;

(** Type of running monitor session
  *)
type t =
    {
      conf:          configuration;
      pid:           pid;
      watchdata_fun: (watch_data -> watch_data) list;
    }
;;

(** {1 Interface } 
  *)

let string_of_exception exc =
  match exc with
    NotImplemented ->
      "Not implemented"
  | _ ->
      raise exc
;;

let create conf =
  let args =
    Array.append [|conf.prog|] conf.args
  in
  let pid =
    match conf.limit_time, conf.limit_mem with 
      | Some _, _ 
      | _, Some _ ->
          (
            let pid = 
              Unix.fork ()
            in
              if pid = 0 then
                (
                  (* TODO: setrlimit; *)
                  Unix.execv
                    conf.prog
                    args
                );
              pid
          )
      | None, None ->
          Unix.create_process 
            conf.prog
            args
            Unix.stdin
            Unix.stdout
            Unix.stderr
  in
  let time_wd_fun =
    let start_time =
      Unix.gettimeofday ()
    in
      fun wd ->
        {wd with timestamp = (Unix.gettimeofday ()) -. start_time}
  in 
  let os_wd_fun =
    if conf.watch_resources then
      if Sys.os_type = "Unix" then
        (
          PML.pid_read conf.watch_children pid
        )
      else
        (
          failwith "No way to watch resource"
        )
    else
      (fun wd -> wd)
  in
  let dirsize_fun =
    if conf.watch_dir <> [] then
      (fun wd ->
         try
           (
             {wd with 
                 dirsize =
                   FileUtil.string_of_size 
                     (FileUtil.size_to_Mo
                        (fst (FileUtil.StrUtil.du conf.watch_dir)))}
           )
         with _ ->
           wd
      )
    else 
      (fun wd -> wd)
  in
    {
      conf          = conf;
      pid           = pid;
      watchdata_fun = [time_wd_fun; os_wd_fun; dirsize_fun];
    }
;;
let poll t =
  match Unix.waitpid [Unix.WNOHANG] t.pid with
    | 0, _ ->
        ProcessLive 
          (List.fold_left 
             (fun wd f -> f wd) 
             ProcessMonitorWatchData.default
             t.watchdata_fun)
    | pid, process_status ->
        ProcessExited process_status
;;

let rec auto_poll t interval f a =
  if interval <= 0.0 then
    invalid_arg 
      (Printf.sprintf "Poll interval must be >= 0.0 (%f)" interval);

  match poll t with 
    | ProcessExited process_status ->
        process_status
    | ProcessLive watch_data ->
        let na =
          f a watch_data
        in
          ignore(Unix.select [] [] [] interval); 
          auto_poll t interval f na
;;
