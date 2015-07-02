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


let World:Links= [(Atlanta , Miami);
                  (Atlanta , Washington);
                  (Washington, NewYork);
                  (MexicoCity,Miami);
                  (Chicago,Atlanta);
                  (Miami, Chicago)]

let toCityInfection = fun (y:City) -> (y,{NbCubes=0})

let CityList = FSharpType.GetUnionCases typeof<City> 
               |> Array.toList
               |> List.map (fun y -> FSharpValue.MakeUnion (y,[||]) :?> City) 

for city in [|Nowhere;Atlanta;Miami;Washington;MexicoCity;Chicago;NewYork|] do
    AreEqual (city, List.find (fun c -> c = city) CityList)


type InfectedWorld = Map<City, InfectionLevel>

let initialInfectedWorld: InfectedWorld = 
    let noInfectionLevel:InfectionLevel = {NbCubes=0}
    CityList
    |> List.map (fun city -> (city, noInfectionLevel))
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
    
let rec PropagateOutbreak (world:Links) (city:City) (infectedWorld:InfectedWorld ) = 
    printfn "Propagate outbreak from %A" city
    printfn "   Infected World %A" infectedWorld

    let SortOutbreakLast (c, lvl) = 
                        match lvl with
                        | Cups _ -> 0
                        | Oups _ -> 1

    NeighborHoodOf world city
    |> List.map (fun y -> (y,InfectCity (infectedWorld.[y])))
    |> List.sortBy SortOutbreakLast 
    |> fun xs -> printfn "%A" xs; xs
    |> List.fold (fun (updatedWorld:InfectedWorld) t -> 
                                let (infectedCity,infectResult) = t 
                                match infectResult with
                                    | Cups levl -> updatedWorld.Add(infectedCity,levl)
                                    | Oups outb -> PropagateOutbreak world infectedCity updatedWorld
                          ) infectedWorld

let noInfection:InfectionLevel = {NbCubes=0}
let oneInfection:InfectionLevel = {NbCubes=1}
    
let miamiInfectedWorld = initialInfectedWorld.Add(Miami, {NbCubes=3})
let propagateFromAtlanta_MiamiOutbreak = PropagateOutbreak World Atlanta miamiInfectedWorld

//
// Infinite loop!!!
//
let miamiAndAtlantaInfectedWorld = initialInfectedWorld
                                            .Add(Miami, {NbCubes=3})
                                            .Add(Atlanta, {NbCubes=3})
let propagateFromAtlanta_MiamiOutbreakChain = PropagateOutbreak World Atlanta miamiAndAtlantaInfectedWorld


let propagateFromAtlanta = PropagateOutbreak World Atlanta initialInfectedWorld
let propagateFromMiami = PropagateOutbreak World Miami initialInfectedWorld

let expectedInfections = [ (Nowhere, noInfection); 
                           (Atlanta, noInfection);
                           (Miami, oneInfection); 
                           (Washington, oneInfection);
                           (MexicoCity, noInfection); 
                           (Chicago, oneInfection);
                           (NewYork, noInfection)] |> Map.ofList
AreEqual (expectedInfections, propagateFromAtlanta)
