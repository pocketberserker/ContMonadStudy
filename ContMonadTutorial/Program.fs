module ContMonadTutorial

let dup x = x * x

type PrintResultHandler () =
  member this.PrintResult(result: int) = printfn "result = %d" result

let dupCps (x: int, resultHandler: PrintResultHandler) =
  let result = x * x
  resultHandler.PrintResult(result)

[<EntryPoint>]
let main _ =
  let x = 10
  let result = dup x
  dupCps(x, PrintResultHandler())
  0
