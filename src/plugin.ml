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
  let keys = Key_sequence.create_exn "C-x C-j" in
  let keymap = Keymap.global () in
  Keymap.define_key keymap keys
    (Command (Command.of_value_exn (Value.intern "bf-eval-buffer")))
;;
