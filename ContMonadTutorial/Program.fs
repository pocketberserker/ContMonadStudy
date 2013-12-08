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

type BinaryValue = {
  left: int
  right: int
}

module CpsTest =

  open Basis.Core

  let parse<'R> : string -> Cps<BinaryValue, 'R> =
    fun value ->
      let (left, right) = value |> Str.split2 ","
      Cps.unit' { left = int left; right = int right}

  let sum<'R> : BinaryValue -> Cps<int, 'R> =
    function {left = left; right = right} -> Cps.unit' (left + right)

let printResult = printfn "result = %d"

let restCps () =
  let result =
    "10,20"
    |> Cps.unit'
    |> Cps.bind CpsTest.parse
    |> Cps.bind CpsTest.sum
    |> Cps.run id
  assert (result = 30)

[<EntryPoint>]
let main _ =
  restCps ()
  0
