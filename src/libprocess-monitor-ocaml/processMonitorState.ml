
(** Possible state of process.
  * @author Sylvain Le Gall
  *)

(** Different possible state. Real possible state is OS dependent.
  *)
type state =
  | Running
  | Sleeping
  | WaitingDisk
  | Zombie
  | Traced
  | Paging
  | StateUnknown
;;

(** Convert a state to string representation 
  *)
let string_of_state = 
  function 
    | Running      -> "running"
    | Sleeping     -> "sleeping in an interruptible wait"
    | WaitingDisk  -> "waiting in uninterruptible disk sleep"
    | Zombie       -> "zombie" 
    | Traced       -> "traced or stopped (on a signal)"
    | Paging       -> "paging"
    | StateUnknown -> "unknown state"
;;

