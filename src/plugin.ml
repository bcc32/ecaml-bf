open Ecaml

let () =
  defun [%here] Value.Type.string (Symbol.intern "bf-eval")
    ~docstring:"evaluate [program], a brainfuck program, given [input]"
    begin
      let open Defun.Let_syntax in
      let%map_open program = required (Symbol.intern "program") Value.Type.string
      and          input   = required (Symbol.intern "input")   Value.Type.string in
      Bf_lib.Program.run' ~program ~input
    end
;;

let () =
  defun [%here] Value.Type.unit (Symbol.intern "bf-eval-buffer")
    ~docstring:"evaluate the current buffer as brainfuck code"
    ~interactive:"sInput: "
    begin
      let open Defun.Let_syntax in
      let%map_open input = required (Symbol.intern "input") Value.Type.string in
      let program =
        Current_buffer.contents ()
        |> Text.to_utf8_bytes
      in
      Bf_lib.Program.run' ~program ~input
      |> messagef "Output: %s"
    end
;;

let mode_name = Symbol.intern "bf-mode"

let () =
  (* define [bf-mode] as a major mode *)
  let mode =
    Major_mode.define_derived_mode ~parent:Major_mode.fundamental
      [%here]
      ~change_command:mode_name
      ~docstring:"Major mode for interacting with brainfuck code."
      ~initialize:ignore
      ~mode_line:"brainfuck"
  in
  (* bind [C-x C-e] to [bf-eval-buffer] *)
  let keymap = Major_mode.keymap mode in
  Keymap.define_key keymap (Key_sequence.create_exn "C-x C-e")
    (Command (Command.of_value_exn (Value.intern "bf-eval-buffer")))
;;

let () =
  (* automatically start [bf-mode] upon opening a [*.b] file *)
  Auto_mode_alist.add Auto_mode_alist.Entry.(
    [ { delete_suffix_and_recur = false
      ; filename_match = Regexp.of_pattern "\\.b\\'"
      ; function_ = Some mode_name } ])
;;

let () = provide (Symbol.intern "ecaml-bf")
