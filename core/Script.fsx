// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "core.fs"

open ginkgopolis.core


let Fail message = failwith message

let IsTrue(success) = 
    if not success then failwith "Expected true"

let AreEqual<'Ta when 'Ta : equality> (expected : 'Ta) (actual : 'Ta) = 
    if not (expected = actual) then sprintf "Expected '%A' Actual '%A'" expected actual |> failwith

let Throws<'T when 'T :> exn>(f) = 
    let fail() = failwith "Expected %s" typeof<'T>.Name
    try 
        f()
        fail()
    with
    | :? 'T as e -> e
    | _ -> fail()

// -----------------------------------------------------------------------------

// TEST - too much player
// TEST - player/playerId interleaved



let shuffled = shuffle initialBuildingTiles

printfn "Tiles shuffled"
printfn "%A" shuffled

let city = (layoutCity shuffled)

let keys = 
    city.layout
    |> Map.toList
    |> List.map (fun (k, v) -> k)

AreEqual 21 (List.length keys)


AreEqual ((-2, -2), (+2, +2)) (rangeOf city.layout)

printCity city