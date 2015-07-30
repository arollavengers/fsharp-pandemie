// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#light
open Microsoft.FSharp.Reflection

let Fail message = failwith message
let IsTrue(success) = if not success then failwith "Expected true"
let AreEqual<'T when 'T:equality>(expected:'T, actual:'T) =
    printfn "Expected.: %A" expected
    printfn "Actual...: %A" actual
    if not (expected = actual) then
        sprintf "Expected '%A' Actual '%A'" expected actual 
        |> failwith
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

// -------------------------------------------------------------------------------------------------------
//
//                                                                         __Essen____StPetersburg
//                             ____ Montreal_____NewYork__________London__/  /  |        |
//  SanFrancisco____Chicago___/         |         /    \           |   \    /   |        |
//        \        /  |   \             |        /      \__        /   Paris___Milan     |
//         \      /   |   \             |       /          \__    |   /   |         \    |
//      LosAngeles    |    Atlanta____Washington              \__Madrid   |          Istanbul
//               \    |        \      /                               \   |      _____/  
//                \   |         \    /                                 Algiers__/
//                 MexicoCity___Miami
//
// -------------------------------------------------------------------------------------------------------

type City =
   | Nowhere
   // blue cities
   | Atlanta
   | Chicago
   | Essen
   | London
   | Madrid
   | Milan
   | NewYork
   | Paris
   | SanFrancisco
   | SaintPetersburg
   | Toronto
   | Washington
   // red cities
   | Tokyo
   | Manila
   // yellow cities
   | LosAngeles
   | MexicoCity
   | Miami
   | SaoPaulo
   // black cities
   | Moscow
   | Istanbul
   | Algiers

type Link = City * City
type Links = Link list

let World:Links= [  (SanFrancisco, Tokyo);
                    (SanFrancisco, Manila);
                    (SanFrancisco, LosAngeles);
                    (SanFrancisco, Chicago);
                    (Chicago, LosAngeles);
                    (Chicago, MexicoCity);
                    (Chicago, Atlanta);
                    (Chicago, Toronto);
                    (Atlanta, Miami);
                    (Atlanta, Washington);
                    (Toronto, Washington);
                    (Toronto, NewYork);
                    (Washington, NewYork);
                    (Washington, Miami);
                    (NewYork, Madrid);
                    (NewYork, London);
                    (Madrid, SaoPaulo);
                    (Madrid, Algiers);
                    (Madrid, Paris);
                    (Madrid, London);
                    (London, Paris);
                    (London, Essen);
                    (Paris, Algiers);
                    (Paris, Milan);
                    (Paris, Essen);
                    (Essen, Milan);
                    (Essen, SaintPetersburg);
                    (Milan, Istanbul);
                    (SaintPetersburg, Istanbul);
                    (SaintPetersburg, Moscow)]

let CityList = FSharpType.GetUnionCases typeof<City>
               |> Array.toList
               |> List.map (fun y -> FSharpValue.MakeUnion (y,[||]) :?> City)

for city in [|Nowhere;Atlanta;Miami;Washington;MexicoCity;Chicago;NewYork|] do
    IsTrue (List.exists (fun c -> c = city) CityList)

type InfectionLevel = {NbCubes : int}
type Outbreak = unit
type InfectedWorld = Map<City, InfectionLevel>

let noInfectionLevel:InfectionLevel = {NbCubes=0}

let initialInfectedWorld: InfectedWorld =
    CityList
    |> List.map (fun city -> (city, noInfectionLevel))
    |> Map.ofList

type InfectResult = Cups of InfectionLevel | Oups of Outbreak

let InfectCity city =
    match city.NbCubes with
    | 3 -> Oups ()
    | _ -> Cups {NbCubes = city.NbCubes + 1}

let NeighborHoodOf (world:Links) (city:City) =
    world
    |> List.filter (fun (a,c) -> c=city || a=city)
    |> List.map (fun x -> match x with
                                  | (a,b) when b=city -> a
                                  | (a,b) when a=city -> b
                                  | (a,b)  -> Nowhere)

let rec PropagateOutbreak (world:Links) (city:City) (infectedWorld:InfectedWorld ) (alreadyOutbreak:City list) =
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
                                    | Cups levl -> updatedWorld.Add(city,levl)
                                    | Oups outb when (List.exists (fun x -> x = city) alreadyOutbreak) -> updatedWorld
                                    | Oups outb ->  PropagateOutbreak world city updatedWorld (city::alreadyOutbreak)
                          ) infectedWorld

// -------------------------------------------------------------------------------------------------------


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
    noInfectionLevel
    |> InfectCity
    |> fun x -> CubeMatcher x 1
    |> InfectCity
    |> fun x -> CubeMatcher x 2
    |> InfectCity
    |> fun x -> CubeMatcher x 3
    |> InfectCity
    |> fun x -> OutbreakMatcher x

let NeighborHoodOfAtlanta =
    NeighborHoodOf World Atlanta
    |> fun xs -> AreEqual(3,List.length xs);xs
    |> fun xs -> 
        let l1:City list = xs
        let l2:City list = [Miami;Washington;Chicago]
        AreEqual(l1,l1);xs




let noInfection:InfectionLevel = {NbCubes=0}
let oneInfection:InfectionLevel = {NbCubes=1}
let threeInfection:InfectionLevel = {NbCubes=3}
    
let propagateFromAtlanta:Map<City,InfectionLevel> = PropagateOutbreak World Atlanta initialInfectedWorld []

let expectedInfections:Map<City,InfectionLevel> = 
                         [ (Nowhere, noInfection); 
                           (Atlanta, noInfection);
                           (Miami, oneInfection); 
                           (Washington, oneInfection);
                           (MexicoCity, noInfection); 
                           (Chicago, oneInfection);
                           (NewYork, noInfection)] |> Map.ofList
//AreEqual (expectedInfections, propagateFromAtlanta)


let miamiAndAtlanteInfected = initialInfectedWorld
                                //.Add(Miami, threeInfection) 
                                .Add(Atlanta, threeInfection)
let propagateFromAtlantaWithMiami = PropagateOutbreak World Atlanta miamiAndAtlanteInfected []