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
  isAtEnd : bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Arg =
  let of' v a = { value = v; acc = a; isAtEnd = false }
  let forResult acc = { value = 0; acc = acc; isAtEnd = true }

let rec fact = function
| { value = v; acc = a; isAtEnd = isAtEnd } ->
  if v = 0 then Cps.unit' (Arg.forResult a)
  else (v - 1, a * v) ||> Arg.of' |> Cps.unit'

let testFact () =
  let init = (10, 1) ||> Arg.of' |> Cps.unit'
  let rec loop iter =
    if (iter |> Cps.run id).isAtEnd then iter
    else loop (iter |> Cps.run id |> Cps.unit' |> Cps.bind fact)
  let result =
    init
    |> loop
    |> Cps.run id
  assert (result.acc = 3628800)

[<EntryPoint>]
let main _ =
  testFact ()
  0
