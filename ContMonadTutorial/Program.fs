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

let rec fact =
  fun n ->
    printfn "fact(%d)" n
    if n = 0 then Cps.unit' 1
    else fact (n - 1) |> Cps.bind (((*) n) >> Cps.unit')

let testFact () =
  let result = Cps.unit' 10 |> Cps.bind fact |> Cps.run id
  assert (result = 3628800)

[<EntryPoint>]
let main _ =
  testFact ()
  0
