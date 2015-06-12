// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#light
#load "Component1.fs"
open core

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
type InfectResult = Cups of City | Oups of Outbreak

let InfectCity city =
    match city.NbCubes with
    | 3 -> Oups () 
    | _ -> Cups {NbCubes = city.NbCubes + 1}
    

let NewAtlanta1 = InfectCity Atlanta

let x = match NewAtlanta1 with
                    | Cups city -> InfectCity city
                    | Oups outb -> NewAtlanta1

let NewAtlanta2 = InfectCity NewAtlanta1

AreEqual (1, NewAtlanta.NbCubes)


let NewAtlanta2 = 
    Atlanta 
    |> InfectCity            
    |> fun x -> AreEqual (1, x.NbCubes); x
    |> InfectCity
    |> fun x -> AreEqual (2, x.NbCubes); x
    |> InfectCity
    |> fun x -> AreEqual (3, x.NbCubes); x
    |> InfectCity
    |> fun x -> AreEqual (3, x.NbCubes); x

   
AreEqual (1, NewAtlanta.NbCubes)
