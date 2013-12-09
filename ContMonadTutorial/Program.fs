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

[<EntryPoint>]
let main _ =
  0
