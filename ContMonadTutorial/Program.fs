module ContMonadTutorial

let printResult: int -> unit = printfn "result = %d"

let makeCps (run: (int -> unit) -> unit) = run

let makeDupCps (x: int) : (int -> unit) -> unit =
  let dup = fun resultHandler ->
    let result = x * x
    resultHandler result
  makeCps dup

[<EntryPoint>]
let main _ =
  let x = 10
  makeDupCps x printResult
  0
