
(** Linux specific primitives for monitoring process.
  *)

open ProcessMonitorTypes;;
open ProcessMonitorWatchData;;
open ProcessMonitorState;;
open ProcessMonitorUtils;;

(** Size a memory page 
  *)
let page_size = 
  (* TODO: use sysconf *)
  4096.0
;;

(** Check that process exists *)
let pid_test pid =
  Sys.file_exists ("/proc/"^(string_of_int pid)^"/stat")
;;

(** Merge watch data extracted from related PID
  *)
let pid_read children pid wdata = 

  let pid_read_one pid wdata =
    (** Open files related to given PID
      *)
    let chn = 
      open_in ("/proc/"^(string_of_int pid)^"/stat")
    in

      (* We read proc/pid/stat entry to get main data about the process PID.
       * There is 2 way getting information about this file:
       * - linux kernel source code
       * - man proc
       *
       * Copy from linux-2.6.25/fs/proc/array.c:
       * seq_printf(m, "%d (%s) %c %d %d %d %d %d %u %lu \
       *   %lu %lu %lu %lu %lu %ld %ld %ld %ld %d 0 %llu %lu %ld %lu \
       *   %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %d %d %u %u \
       *   %llu %lu %ld\n",
       *  01       pid_nr_ns(pid, ns),
       *  02       tcomm,
       *  03       state,
       *  04       ppid,
       *  05       pgid,
       *  06       sid,
       *  07       tty_nr,
       *  08       tty_pgrp,
       *  09       task->flags,
       *  10       min_flt,
       *  11       cmin_flt,
       *  12       maj_flt,
       *  13       cmaj_flt,
       *  14       cputime_to_clock_t(utime),
       *  15       cputime_to_clock_t(stime),
       *  16       cputime_to_clock_t(cutime),
       *  17       cputime_to_clock_t(cstime),
       *  18       priority,
       *  19       nice,
       *  20       num_threads,
       *  21       start_time,
       *  22       vsize,
       *  23       mm ? get_mm_rss(mm) : 0,
       *  24       rsslim,
       *  25       mm ? mm->start_code : 0,
       *  26       mm ? mm->end_code : 0,
       *  27       mm ? mm->start_stack : 0,
       *  28       esp,
       *  29       eip,
       *         /* The signal information here is obsolete.
       *          * It must be decimal for Linux 2.0 compatibility.
       *          * Use /proc/#/status for real-time signals.
       *          */
       *  30       task->pending.signal.sig[0] & 0x7fffffffUL,
       *  31       task->blocked.sig[0] & 0x7fffffffUL,
       *  32       sigign      .sig[0] & 0x7fffffffUL,
       *  33       sigcatch    .sig[0] & 0x7fffffffUL,
       *  34       wchan,
       *  35       0UL,
       *  36       0UL,
       *  37       task->exit_signal,
       *  38       task_cpu(task),
       *  39       task->rt_priority,
       *  40       task->policy,
       *  41       (unsigned long long)delayacct_blkio_ticks(task),
       *  42       cputime_to_clock_t(gtime),
       *  43       cputime_to_clock_t(cgtime));
       *) 
    let stat = 
      seek_in chn 0;
      input_line chn
    in

    let stat_splitted =

      let rec split_blank acc i =
        if i < String.length stat then
          (
            try
              let next = 
                String.index_from stat i ' '
              in
                split_blank 
                  ((String.sub stat i (next - i)) :: acc)
                  (next + 1)
            with Not_found ->
              let lst =
                String.sub stat i ((String.length stat) - i)
              in
                split_blank (lst :: acc) (String.length stat)
          )
        else
          (
            (Array.of_list (List.rev acc))
          )
      in

        split_blank [] 0
    in

    let rpid =
      int_of_string stat_splitted.(0)
    in
    let cstate =
      stat_splitted.(2)
    in
    let num_threads = 
      int_of_string stat_splitted.(19)
    in
    let vsize =
      Int64.of_string stat_splitted.(22)
    in
    let vmrss =
      (float_of_string stat_splitted.(23)) *. page_size
    in
    let cpu =
      int_of_string stat_splitted.(38)
    in
    let state =
      match cstate with 
        | "R" -> Running 
        | "S" -> Sleeping
        | "D" -> WaitingDisk
        | "Z" -> Zombie
        | "T" -> Traced
        | "W" -> Paging
        | _   -> StateUnknown
    in
      assert (rpid = pid);
      close_in chn;
      {
        wdata with 
            vmsize    = (Int64.to_float vsize) +. wdata.vmsize;
            vmrss     = vmrss +. wdata.vmrss;
            thread    = num_threads + wdata.thread;
            cpu       = add_pid_data pid cpu wdata.cpu;
            state     = add_pid_data pid state wdata.state;
      }
  in

  let pid_children pid =
    let all_pid = 
      List.fold_left 
        (fun acc s ->
           try 
             (int_of_string s) :: acc
           with Failure _ ->
             acc)
        [] 
        (Array.to_list (Sys.readdir "/proc/"))
    in

    let ppid_of_pid pid =
      let chn = 
        open_in ("/proc/"^(string_of_int pid)^"/stat")
      in

      let buff_scan = 
        seek_in chn 0;
        Scanf.Scanning.from_channel chn
      in
        Scanf.bscanf
          buff_scan 
          "%_d (%_s@) %_c %d"
          (fun ppid ->
             close_in chn;
             ppid)
    in

    let children_of_pid =
      List.fold_left
        (fun mp pid ->
           try
             let ppid =
               ppid_of_pid pid
             in
               map_pid_push ppid pid mp
           with _ ->
             mp)
        MapPid.empty
        all_pid
    in

    let rec pid_self_children_aux acc pid =
      let acc =
        pid :: acc
      in
        try
          let children =
            MapPid.find pid children_of_pid
          in
            List.fold_left
              pid_self_children_aux 
              acc
              children
        with Not_found ->
          acc
    in

      pid_self_children_aux [] pid
  in

  let pid_lst =
    if children then
      pid_children pid
    else
      [pid]
  in

    List.fold_left
      (fun wd pid -> pid_read_one pid wd)
      wdata
      pid_lst
;;
