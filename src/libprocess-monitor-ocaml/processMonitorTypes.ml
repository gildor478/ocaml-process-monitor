

(** Types and exception for ocaml-process-monitor.
    @author Sylvain Le Gall
  *)

(** {1 Core types} *)

(** Process identififer
  *)
type pid = int
;;

(** CPU identifier
  *)
type cpu_number = int
;;

(** Size expressed in byte
  *)
type size_byte = float
;;

(** Time expressed in second
  *)
type time_second = float
;;

(** Directory name
  *)
type dirname = string
;;

(** File name
  *)
type filename = string
;;

(** {1 Exception} *)

(** Trying to use a feature which is not implementated. 
  *)
exception NotImplemented;;

