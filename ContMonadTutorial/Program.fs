module ContMonadTutorial

let printResult: int -> unit = printfn "result = %d"

let dupCps: int -> unit = fun x ->
  let result = x * x
  printResult result

[<EntryPoint>]
let main _ =
  let x = 10
  dupCps x
  0
