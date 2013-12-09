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

  let callCC f =
    Cps (fun k -> f (fun value -> Cps (fun _ -> k value)) |> run k)

  type CpsBuilder internal () =
    member this.Bind(x, f) = bind f x
    member this.Return(x) = unit' x
    member this.ReturnFrom(x) = x

  let cps = new CpsBuilder()

type BinaryValue = {
  left : int
  right : int
}

module CpsTest =

  let sum = function
  | { left = l; right = r } -> Cps.unit' (l + r)

open Basis.Core

let safetyParse exit =
  fun value ->
    let components = value |> Str.splitBy ","
    if components |> Array.length < 2 then
      exit "*** Error: too few numbers."
    else
      try
        let left = components.[0] |> int
        let right = components.[1] |> int
        Cps.unit' { left = left; right = right }
      with
        | e -> exit ("*** Error: invalid number format:" + e.Message)

let printResult = printfn "%s"
let format = fun value -> Cps.unit' ("result = " + value.ToString())

[<EntryPoint>]
let main _ =
  Cps.callCC (fun exit -> Cps.cps {
    let! a = safetyParse exit "1020"
    let! b = CpsTest.sum a
    return! (format b)
  })
  |> Cps.run printResult
  0
