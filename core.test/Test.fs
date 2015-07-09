module core.test
open System
open NUnit.Framework

open ginkgopolis.core


[<TestFixture>]
type Test() = 

    [<Test>]
    member x.``mapPlayers should fail when trying to map more than 5 players ``() =
        match mapPlayers [ "John"; "Carmen"; "Pacman"; "Flibuste"; "Colin"; "Martin" ] AllPlayerIds Map.empty with
            | Error(TooMuchPlayer ps) -> Assert.AreEqual([ "Martin" ], ps)
            | Error x -> Assert.Fail(sprintf "Wrong error: %A" x)
            | Success _ -> Assert.Fail "More than 5 players should bot be allowed"



    [<Test>]
    member x.``mapPlayers should associate player with playerId``() =
        match mapPlayers [ "Carmen"; "Pacman" ] AllPlayerIds Map.empty with
            | Error x -> Assert.Fail(sprintf "No error expected: %A" x)
            | Success mapped -> 
                let expected = 
                    [ ("Carmen", Player1)
                      ("Pacman", Player2) ]
                    |> Map.ofList
                Assert.AreEqual (expected, mapped)

    [<Test>]
    member x.``should not update world when the triggered action does not match character action``() =
        let character1OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
        let game = newGame [ "John"; "Carmen" ] []
        Assert.AreEqual (game, ((character1OnAction ActionKind.EndGame) Player1 game))
        Assert.AreEqual (game, ((character1OnAction ActionKind.FloorConstruction) Player1 game))




    [<Test>]
    member x.``should generate an error when trying to increase resource when none are available``() =
        let character1OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
        let game = newGame [ "John"; "Carmen" ] []
        let ng = (character1OnAction ActionKind.Urbanization) Player1 game
        match (withinPlayerStateOf Player1 ng (fun p -> p.nbResource)) with
        | Error e -> Assert.AreEqual (NoResourceAvailable, e)
        | Success v -> Assert.Fail(sprintf "And error should have occured, got: %A" v)



    [<Test>]
    member x.``should increase resource``() =
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = increase IncreasableItem.AvailableResource Player2 ng0
        let ng2 = increase IncreasableItem.AvailableResource Player2 ng1

        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbResourceAvailable)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(2, v)


    [<Test>]
    member x.``should increase resource when trying to increase resource and available resource are enough``() =
        let character1OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = increase IncreasableItem.AvailableResource Player2 ng0
        let ng2 = (character1OnAction ActionKind.Urbanization) Player2 ng1
        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbResource)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(1, v)

