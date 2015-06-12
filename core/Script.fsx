// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#light
#load "Component1.fs"
open core

let Fail message = failwith message
let IsTrue(success) = if not success then failwith "Expected true"
let AreEqual(expected, actual) =
    if not (expected = actual) then 
        sprintf "Expected '%A' Actual '%A'" expected actual |> failwith
let Throws<'T when 'T :> exn> (f) =
    let fail () = failwith "Expected %s" typeof<'T>.Name
    try f (); fail () with :? 'T as e -> e | _ -> fail()

// -------------------------------------------------------------------------------

type City = {NbCubes : int} 
type Outbreak = unit

// Atlanta|Miami|Washington|MexicoCity|Chicago

let Atlanta = {NbCubes=0}
type InfectResult = Cups of City| Oups of Outbreak

let InfectCity city =
    match city.NbCubes with
    | 3 -> Oups ()
    | _ -> Cups {NbCubes = city.NbCubes + 1}
    

let NewAtlanta1 = InfectCity Atlanta
match NewAtlanta1 with
            | Cups city -> AreEqual (1, city.NbCubes)
            | Oups outb -> Fail "No Outbreak expected"


let CubeMatcher infectResult nbCubesExpected = 
        match infectResult with
                    | Cups city -> AreEqual (nbCubesExpected, city.NbCubes); city
                    | Oups outb -> Fail "No Outbreak expected"

let OutbreakMatcher infectResult = 
        match infectResult with
                    | Cups city -> Fail "Outbreak expected"
                    | Oups outb -> false// nothing to return


let NewAtlanta2 = 
    Atlanta 
    |> InfectCity            
    |> fun x -> CubeMatcher x 1
    |> InfectCity
    |> fun x -> CubeMatcher x 2
    |> InfectCity
    |> fun x -> CubeMatcher x 3
    |> InfectCity
    |> fun x -> OutbreakMatcher x
