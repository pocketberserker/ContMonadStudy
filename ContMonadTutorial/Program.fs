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

type Arg = {
  value : int
  acc : int
}

let rec fact = function
| { value = v; acc = a } ->
  if v = 0 then Cps.unit' { value = v; acc = a }
  else { value = v - 1; acc = a * v } |> Cps.unit'

let testFact () =
  let result =
    { value = 10; acc = 1 }
    |> Cps.unit'
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.bind fact
    |> Cps.run id
  assert (result.acc = 3628800)

[<EntryPoint>]
let main _ =
  testFact ()
  0
