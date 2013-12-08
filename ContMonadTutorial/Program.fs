module ContMonadTutorial


type Cps<'T, 'R> =
  | Cps of run: (('T -> 'R) -> 'R)

module Cps =

  let private makePassthrough value =
    fun k -> k value

  let unit' value =
    let passthrough = makePassthrough value
    Cps passthrough

  let run k (Cps x) = x k

let printResult: int -> unit = printfn "result = %d"

let makeDupCps (x: int) : Cps<int, unit> =
  let dup =
    fun resultHandler ->
      let result = x * x
      resultHandler result
  Cps dup

[<EntryPoint>]
let main _ =
  let x = 10
  makeDupCps x
  |> Cps.run printResult
  0
