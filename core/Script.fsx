// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#light
open Microsoft.FSharp.Reflection

let Fail message = failwith message
let IsTrue(success) = if not success then failwith "Expected true"
let AreEqual(expected, actual) =
    if not (expected = actual) then 
        sprintf "Expected '%A' Actual '%A'" expected actual |> failwith
let Throws<'T when 'T :> exn> (f) =
    let fail () = failwith "Expected %s" typeof<'T>.Name
    try f (); fail () with :? 'T as e -> e | _ -> fail()

let rec __filter list predicate = 
    match list with
    | [] -> []
    | head :: tail -> match predicate head with 
                      | true -> head :: __filter tail predicate
                      | false -> __filter tail predicate
                        
let rec __map list mapper = 
    match list with
    | [] -> []
    | head :: tail -> mapper head :: __map tail mapper

// -------------------------------------------------------------------------------

type InfectionLevel = {NbCubes : int} 
type Outbreak = unit
type City = Nowhere|Atlanta|Miami|Washington|MexicoCity|Chicago|NewYork
type Link = City * City
type Links = Link list


let World:Links= [(Atlanta , Miami);(Atlanta , Washington);(Washington, NewYork);(MexicoCity,Miami);(Chicago,Atlanta)]

let toCityInfection = fun (y:City) -> (y,{NbCubes=0})

let CityList = FSharpType.GetUnionCases typeof<City> 
               |> Array.toList
               |> fun x -> __map x (fun y -> FSharpValue.MakeUnion (y,[||]):?>City) 


type InfectedWorld = Map<City, InfectionLevel>
let initialInfectedWorld: InfectedWorld = 
    let initialInfectionLevel:InfectionLevel = {NbCubes=0}
    CityList
    |> fun xs -> __map xs (fun city -> (city, initialInfectionLevel))
    |> Map.ofList
    
type InfectResult = Cups of InfectionLevel | Oups of Outbreak

let InfectCity city =
    match city.NbCubes with
    | 3 -> Oups ()
    | _ -> Cups {NbCubes = city.NbCubes + 1}


let AtlantaInfectionLevel = {NbCubes=0}

let NewAtlanta1 = InfectCity AtlantaInfectionLevel
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
    AtlantaInfectionLevel 
    |> InfectCity            
    |> fun x -> CubeMatcher x 1
    |> InfectCity
    |> fun x -> CubeMatcher x 2
    |> InfectCity
    |> fun x -> CubeMatcher x 3
    |> InfectCity
    |> fun x -> OutbreakMatcher x

                    
let NeighborHoodOf (world:Links) (city:City) = 
    world
    |> List.filter (fun (a,c) -> c=city || a=city)
    |> List.map (fun x -> match x with
                                  | (a,b) when b=city -> a
                                  | (a,b) when a=city -> b
                                  | (a,b)  -> Nowhere)
                        
let NeighborHoodOfAtlanta = 
    NeighborHoodOf World Atlanta 
    |> fun xs -> AreEqual(3,List.length xs);xs
    |> fun xs -> AreEqual([Miami;Washington;Chicago],xs);xs
    
let PropagateOutbreak (world:Links, city:City, infectedWorld:InfectedWorld ) = 
    NeighborHoodOf world city
    |> fun xs -> __map xs (fun y -> (y,InfectCity (infectedWorld.[y])))
    |> fun xs -> List.fold (fun updatedWorld t -> 
                                let (city,infectResult) = t 
                                match infectResult with
                                    | Cups levl -> updatedWorld //.Add(city,levl)
                                    | Oups outb -> updatedWorld
                          ), infectedWorld, xs


PropagateOutbreak (World ,Atlanta ,initialInfectedWorld)
PropagateOutbreak (World ,Atlanta ,initialInfectedWorld)