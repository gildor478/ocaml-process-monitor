
(** Small utility to simulate behavior
  *)

let () = 
  let mega = 
    1024 * 1024 / (Sys.word_size / 8)
  in
  let mymemory =
    ref [||]
  in
  let mychildren = 
    ref []
  in
  let rmemory =
    ref 0
  in
  let rfork_depth =
    ref 1
  in
  let rfork_sleep = 
    ref 0.0
  in
    Arg.parse
      [
        "--memory",
        Arg.Int (fun i -> 
                   mymemory := Array.make (i * mega) 0; 
                   rmemory  := i),
        "size_mega Memory to use";

        "--fork-children",
        Arg.Unit 
          (fun () -> 
             if !rfork_depth > 0 then
               let child_cmd =
                 Printf.sprintf 
                   "%s --memory %d --fork-depth %d --fork-sleep %f --fork-children --sleep %f"
                   Sys.executable_name
                   !rmemory
                   (!rfork_depth - 1)
                   !rfork_sleep
                   !rfork_sleep
               in
                 mychildren := (Unix.open_process_out child_cmd) :: !mychildren),
        "nbr Number of children of the process";
        
        "--fork-depth",
        Arg.Set_int rfork_depth,
        "dpth Depth of the process generation";

        "--fork-sleep",
        Arg.Set_float rfork_sleep,
        "second Sleep parameter passed to children";

        "--sleep",
        Arg.Float (fun f -> ignore (Unix.select [] [] [] f)),
        "second Time to wait for";
      ]
      ignore 
      "test-emulator written by Sylvain Le Gall";
    List.iter (fun chn -> ignore(Unix.close_process_out chn)) !mychildren
;;

