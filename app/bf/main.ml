open Bf_lib

let main file =
  let input   = Bf_lib.Input.of_chan stdin            in
  let output  = Bf_lib.Output.of_chan stdout          in
  let memory  = Bf_lib.Memory.create_fresh 10_000_000 in
  let program = Bf_lib.Input.of_chan (open_in file)   in
  Program.parse program
  |> Program.run ~input ~output ~memory
;;

let () =
  let file = Sys.argv.( 1 ) in
  main file
;;
