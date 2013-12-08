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

  let bind f cps =
    Cps (fun k -> cps |> run (fun value -> f value |> run k))

let dup = fun value -> Cps.unit' (value * value)

let printResult: int -> unit = printfn "result = %d"

[<EntryPoint>]
let main _ =
  let x = 10
  x |> Cps.unit' |> Cps.bind dup |> Cps.run printResult
  0
