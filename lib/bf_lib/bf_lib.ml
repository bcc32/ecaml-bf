open! Core_kernel

module Input = struct
  (* [t]'s return None at the end of input. *)
  type t = unit -> char option

  let of_string str =
    let i = ref 0 in
    fun () ->
      if !i >= String.length str
      then None
      else (
        let char = str.[ !i ] in
        incr i;
        Some char)
  ;;

  let of_chan chan = fun () -> In_channel.input_char chan

  let rec iter t ~f =
    match t () with
    | None   -> ()
    | Some c -> f c; iter t ~f
  ;;
end

module Output = struct
  type t = char -> unit

  let of_buffer buf = fun char -> Buffer.add_char buf char

  let of_chan chan = Pervasives.output_char chan
end

module Memory = Bigstring

module Program = struct
  module Command = struct
    type t =
      | Left
      | Right
      | Decrement
      | Increment
      | Input
      | Output
      | Loop_begin
      | Loop_end

    let of_char = function
      | '<' -> Some Left
      | '>' -> Some Right
      | '-' -> Some Decrement
      | '+' -> Some Increment
      | ',' -> Some Input
      | '.' -> Some Output
      | '[' -> Some Loop_begin
      | ']' -> Some Loop_end
      | _   -> None
    ;;
  end

  type t =
    { commands   : Command.t array   (* just the sequence of commands *)
    ; jump_table : int Int.Table.t } (* maps matching brackets' positions to
                                        each others' *)

  let heap_size = 1_000_000

  let extra_left = invalid_argf "unmatched left bracket at command index %d"
  let extra_right = invalid_argf "unmatched right bracket at command index %d"

  let parse source_code =
    (* a stack *)
    let open_brackets = ref [] in
    let commands      = ref [] in
    let current_pos   = ref 0  in
    let jump_table = Int.Table.create () in
    let add_command cmd =
      commands := cmd :: !commands;
      incr current_pos
    in
    let push_left_bracket () =
      open_brackets := !current_pos :: !open_brackets;
      add_command Command.Loop_begin
    in
    let pop_left_bracket () =
      match !open_brackets with
      | []     -> extra_right !current_pos ()
      | hd::tl ->
        Hashtbl.add_exn jump_table ~key:!current_pos ~data:hd;
        Hashtbl.add_exn jump_table ~key:hd ~data:!current_pos;
        open_brackets := tl;
        add_command Command.Loop_begin
    in
    Input.iter source_code ~f:(fun char ->
      match Command.of_char char with
      (* ignore non-command characters *)
      | None -> ()

      (* regular commands just get appended *)
      | Some (Left | Right | Decrement | Increment | Input | Output as cmd)
        -> add_command cmd

      (* record loop beginning positions *)
      | Some Loop_begin -> push_left_bracket ()

      (* record entries in jump table *)
      | Some Loop_end -> pop_left_bracket ());

    (* check for any remaining open left brackets *)
    (match !open_brackets with
     | [] -> ()
     | hd :: _ -> extra_left hd ());
    { commands = Array.of_list_rev !commands
    ; jump_table }
  ;;

  let run t ~memory ~input ~output =
    (* get/set bytes in memory; bytes are unsigned, wrap around on overflow *)
    let get pos   = Memory.get_uint8 memory ~pos           in
    let set pos x = Memory.set_uint8 memory ~pos (x % 256) in

    (* we can actually always jump one past the matching bracket *)
    let jump pc   = Hashtbl.find_exn t.jump_table pc + 1   in

    (* pc  :: program counter
       pos :: data pointer *)
    let rec loop pc pos =
      match t.commands.( pc ) with
      | Left -> loop (pc + 1) (pos - 1)
      | Right -> loop (pc + 1) (pos + 1)
      | Decrement -> set pos (get pos - 1)
      | Increment -> set pos (get pos + 1)

      | Input ->
        (match input () with
         | None   -> ()         (* do nothing if we run [,] at EOF *)
         | Some c -> set pos (Char.to_int c));
        loop (pc + 1) pos

      | Output ->
        output (Char.of_int_exn (get pos));
        loop (pc + 1) pos

      | Loop_begin ->
        if get pos = 0
        then (loop (jump pc + 1) pos)
        else (loop (pc + 1) pos)

      | Loop_end ->
        if get pos <> 0
        then (loop (jump pc + 1) pos)
        else (loop (pc + 1) pos)

      | exception _ -> ()       (* reached end of commands, so we terminate *)
    in
    loop 0 0
  ;;

  let run' ~program ~input =
    let t = parse (Input.of_string program) in
    let buffer = Buffer.create 16 in
    let memory = Memory.create heap_size in
    let input = Input.of_string input in
    let output = Output.of_buffer buffer in
    run t ~memory ~input ~output;
    Buffer.contents buffer
  ;;
end
