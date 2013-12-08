module ContMonadTutorial

let printResult: int -> unit = printfn "result = %d"

let makeDupCps (x: int) : (int -> unit) -> unit =
  fun resultHandler ->
    let result = x * x
    resultHandler result

[<EntryPoint>]
let main _ =
  let x = 10
  makeDupCps x printResult
  0
