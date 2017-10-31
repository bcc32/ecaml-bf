open! Core_kernel

module Input : sig
  (** Stateful input source. *)
  type t

  val of_string : string -> t

  val of_chan : In_channel.t -> t
end

module Output : sig
  (** Stateful output sink. *)
  type t

  val of_buffer : Buffer.t -> t

  val of_chan : Out_channel.t -> t
end

module Memory : sig
  (** A big byte array. *)
  type t = Bigstring.t
end

module Program : sig
  (** An immutable representation of a brainfuck program. *)
  type t

  (** "Compile" a program from its source code. *)
  val parse : Input.t -> t

  (** [run] mutates [memory], consumes [input], and writes to [output]. *)
  val run
    :  t
    -> memory : Memory.t
    -> input  : Input.t
    -> output : Output.t
    -> unit

  (** A simple interface for [run]. *)
  val run'
    :  program : string
    -> input   : string
    -> string                   (* output *)
end
