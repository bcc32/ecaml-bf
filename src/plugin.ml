open Ecaml

let eval_program program input =
  let program = Value.to_utf8_bytes_exn program in
  let input   = Value.to_utf8_bytes_exn input   in
  Bf_lib.Program.run' ~program ~input
  |> Value.of_utf8_bytes
;;

let () =
  defun [%here] (Symbol.intern "bf-eval")
    ~docstring:"evaluate [program], a brainfuck program, given [input]"
    ~args:[ Symbol.intern "program"
          ; Symbol.intern "input" ]
    (function
      | [| program; input |] -> eval_program program input
      | _ -> invalid_arg "wrong arity")
;;

let eval_current_buffer input =
  let program =
    Current_buffer.contents ()
    |> Text.to_utf8_bytes
  in
  let input = Value.to_utf8_bytes_exn input in
  Bf_lib.Program.run' ~program ~input
;;

let () =
  defun [%here] (Symbol.intern "bf-eval-buffer")
    ~docstring:"evaluate the current buffer as brainfuck code"
    ~interactive:"sInput: "
    ~args:[ Symbol.intern "input" ]
    (function
      | [| input |] ->
        let output = eval_current_buffer input in
        messagef "Output: %s" output;
        Value.nil
      | _ -> invalid_arg "wrong arity")
;;

let () =
  (* define [bf-mode] as a major mode *)
  let mode =
    Major_mode.define_derived_mode ~parent:Major_mode.fundamental
      [%here]
      ~change_command:(Symbol.intern "bf-mode")
      ~docstring:"Major mode for interacting with brainfuck code."
      ~initialize:ignore
      ~mode_line:"brainfuck"
  in
  (* bind [C-x C-e] to [bf-eval-buffer] *)
  let keymap = Major_mode.keymap mode in
  Keymap.define_key keymap (Key_sequence.create_exn "C-x C-e")
    (Command (Command.of_value_exn (Value.intern "bf-eval-buffer")))
;;

let () = provide (Symbol.intern "ecaml-bf")
