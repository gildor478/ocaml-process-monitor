
open OUnit;;
open ProcessMonitorTypes;;
open ProcessMonitor;;
module WatchData = ProcessMonitorWatchData;;

let _ = 
  (* Location of the test-emulator binary *)
  let test_emulator = 
    let basedir =
      Filename.dirname Sys.executable_name 
    in
    let native = 
      Filename.concat basedir "test-emulator.native"
    in
    let byte = 
      Filename.concat basedir "test-emulator.byte"
    in
      if Sys.file_exists native then
        native
      else
        byte
  in

  (* Default process monitor configuration *)
  let conf_default =
    {
      prog            = test_emulator;
      args            = [||];
      watch_resources = false;
      watch_children  = false;
      watch_dir       = None;
      limit_mem       = None;
      limit_time      = None;
    }
  in

    run_test_tt_main
      ("ocaml-process-monitor" >:::
       [
         "simple-run" >::
         (fun () ->
            let t =
              create conf_default
            in
              assert_equal 
                (auto_poll t 0.1 (fun _ _ -> ()) ())
                (Unix.WEXITED 0)
         )
       ]
      )

;;
