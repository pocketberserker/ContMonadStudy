module ContMonadTutorial

let dup x = x * x

[<EntryPoint>]
let main _ =
  let x = 10
  let result = dup x
  printfn "result = %d" result
  0
