// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#light
open Microsoft.FSharp.Reflection

let AreEqual(expected, actual) =
    printfn "Expected.: %A" expected
    printfn "Actual...: %A" actual
    if not (expected = actual) then
        sprintf "Expected '%A' Actual '%A'" expected actual 
        |> failwith

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
