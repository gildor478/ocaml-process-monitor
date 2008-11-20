
(** Watch data 
  * @author Sylvain Le Gall
  *)

open ProcessMonitorTypes;;
open ProcessMonitorState;;
open ProcessMonitorUtils;;

(** Data returned by poll. 
  *
  * If watch_resources is turned on, data are collected using OS specific
  * scheme otherwise it is a default data structure with only timestamp
  * initialized.
  *
  * If watch_children is turned on, data are cumulative through children and
  * parent process otherwise no data are collected from children. 
  *
  * [pid] that can be found in [cpu] and [state] are always linked by a
  * parent/children relation.
  *)
type watch_data =
  {
    (** Time since begin of monitoring
      *)
    timestamp: time_second;

    (** Virtual memory size
      *)
    vmsize: size_byte;
    
    (** Resident Set Size: number of pages the process has in real memory
      *)
    vmrss: size_byte;

    (** Number of threads
      *)
    thread: int;

    (** List of processor on which correspondig PID has been executed last.
      *)
    cpu: cpu_number MapPid.t;

    (** Size of the directory watched
      *)
    dirsize: string;

    (** Files in the directory watched
      *)
    dirfiles: string list;

    (** Event send by the application (when application is aware of
      * ocaml-process-monitor)
      *)
    event: string MapPid.t;

    (** State foreach process watched
      *)
    state: state MapPid.t;
  }
;;

let default =
  {
    timestamp = 0.0;
    vmsize    = 0.0;
    vmrss     = 0.0;
    thread    = 0;
    cpu       = MapPid.empty;
    dirsize   = "0B";
    dirfiles  = [];
    event     = MapPid.empty;
    state     = MapPid.empty;
  }
;;

let add_pid_data pid data mp =
  MapPid.add pid data mp
;;

let to_string t =
  let pid_map_string f () mp =
    let lst = 
      MapPid.fold
        (fun pid data acc -> (Printf.sprintf "%d -> %s" pid (f data)) :: acc)
        mp 
        []
    in
      "["^(String.concat "; " lst)^"]"
  in
    Printf.sprintf 
      "{\
      timestamp: %fs;\n  \
      vmsize:    %s;\n  \
      vmrss:     %s;\n  \
      thread:    %d;\n  \
      cpu:       %a;\n  \
      dirsize:   %s;\n  \
      event:     %a;\n  \
      state:     %a;\n\
      }"
      t.timestamp
      (string_of_bytes t.vmsize)
      (string_of_bytes t.vmrss)
      t.thread 
      (pid_map_string string_of_int) t.cpu    
      t.dirsize
      (pid_map_string (fun s -> s)) t.event  
      (pid_map_string string_of_state) t.state  
;;
