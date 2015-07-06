// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
open Microsoft.FSharp.Reflection

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

// -------------------------------------------------------------------------------
type TwoTrack<'TEntity, 'TError> = 
    | Success of 'TEntity
    | Error of 'TError

// -------------------------------------------------------------------------------
type ResourceColor = 
    | Blue
    | White
    | Orange
    | LightBrown
    | Green


type SuccessPoint = unit

type UrbanizationToken = 
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L

type UrbanizationCard = UrbanizationToken

type BuildingColor = 
    | Blue
    | Red
    | Yellow

type BuildingNum = int

type BuildingTile = 
    { color : BuildingColor
      number : BuildingNum }

type BuildingCard = BuildingTile

type Card = 
    | UrbanizationCard of UrbanizationCard
    | BuildingCard of BuildingCard

type GreenSpaceTile = unit

type Tile = 
    | BuildingTile of BuildingTile
    | GreenSpaceTile of GreenSpaceTile

type CharacterId = int

type CharacterGroup = 
    | NoGroup
    | Group1
    | Group2
    | Group3
    | Group4
    | Group5
    | Group6
    | Expert

type Event = 
    | DesignCompetition
    | AdvertisingCampaign
    | Expropriation
    | QualityControl

type InitialItem = 
    | Resource = 0
    | SuccessPoint = 1
    | BuildingTile = 2
    | Event = 3

type Player = string

type PlayerId = 
    | Player1
    | Player2
    | Player3
    | Player4
    | Player5

let AllPlayerIds = [ Player1; Player2; Player3; Player4; Player5 ]

type PlayerState = 
    { nbResource : int
      nbResourceAvailable : int
      nbSuccessPoint : int
      cards: BuildingCard list
      characters : CharacterId list
      tiles : Tile list }

type Game = 
    { playersToIds : Map<Player, PlayerId>
      playerStates : Map<PlayerId, PlayerState>
      availableTiles : Tile list }

type GameError = 
    | TooMuchPlayer of Player list
    | UnknownPlayer of Player
    | PlayerIdNotBound of PlayerId
    | NoResourceAvailable
    | NoTileAvailableInGame
    | UnknownIncreasableItem

let rec mapPlayers (players : Player list) (playerIds : PlayerId list) (mapped : Map<Player, PlayerId>) = 
    match playerIds with
    | id :: ids -> 
        match players with
        | p :: ps -> mapPlayers ps ids (mapped.Add(p, id))
        | [] -> Success mapped
    | [] -> Error(TooMuchPlayer players)

// TEST
match mapPlayers [ "John"; "Carmen"; "Pacman"; "Flibuste"; "Colin"; "Martin" ] AllPlayerIds Map.empty with
| Error(TooMuchPlayer ps) -> AreEqual([ "Martin" ], ps)
| Error x -> Fail(sprintf "Wrong error: %A" x)
| Success _ -> Fail "More than 5 players should bot be allowed"

let newGame (players : Player list) (availableTiles : Tile list) : TwoTrack<Game, GameError> = 
    match mapPlayers players AllPlayerIds Map.empty with
    | Error err -> Error err
    | Success ids -> 
        let initialPlayerState = 
            { nbResource = 0
              nbResourceAvailable = 0
              nbSuccessPoint = 0
              cards = []
              characters = []
              tiles = [] }
        
        let playerStates = 
            ids
            |> Map.toList
            |> List.map (fun (p, id) -> (id, initialPlayerState))
            |> Map.ofList
        
        Success { playersToIds = ids
                  playerStates = playerStates
                  availableTiles = availableTiles }

let game0 = newGame [ "John"; "Carmen" ] []

type ActionKind = 
    | Urbanization = 1
    | FloorConstruction = 2

type UpdateGame = PlayerId -> TwoTrack<Game, GameError> -> TwoTrack<Game, GameError>

let identityUpdateGame : UpdateGame = fun (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) -> gameTT

type OnAction = ActionKind -> UpdateGame

type IncreasableItem = 
    | Resource = 0
    | AvailableResource = 1
    | SuccessPoint = 2
    | BuildingTile = 3

let increase (item : IncreasableItem) (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) = 
    match gameTT with
    | Error _ -> gameTT
    | Success game -> 
        let { playerStates = ps; availableTiles = ts } : Game = game
        match ps.TryFind(player) with
        | None -> Error(PlayerIdNotBound player)
        | Some playerState -> 
            let playerState = ps.[player]
            match item with
            | IncreasableItem.Resource -> 
                if playerState.nbResourceAvailable > 0 then 
                    let newState = 
                        { playerState with nbResource = playerState.nbResource + 1
                                           nbResourceAvailable = playerState.nbResourceAvailable - 1 }
                    Success { game with playerStates = ps.Add(player, newState) }
                else Error NoResourceAvailable
            | IncreasableItem.AvailableResource -> 
                let newState = { playerState with nbResourceAvailable = playerState.nbResourceAvailable + 1 }
                Success { game with playerStates = ps.Add(player, newState) }
            | IncreasableItem.SuccessPoint -> 
                let newState = { playerState with nbSuccessPoint = playerState.nbSuccessPoint + 1 }
                Success { game with playerStates = ps.Add(player, newState) }
            | IncreasableItem.BuildingTile -> 
                match ts with
                | [] -> Error NoTileAvailableInGame
                | tile :: remainings -> 
                    let newState = { playerState with tiles = tile :: playerState.tiles }
                    Success { game with playerStates = ps.Add(player, newState)
                                        availableTiles = remainings }
            | _ -> Error UnknownIncreasableItem

let whenAction (requiredKind : ActionKind) (updater : UpdateGame) = 
    fun (kind : ActionKind) -> 
        if kind = requiredKind then updater
        else identityUpdateGame

let funChar1 = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
let ng = (funChar1 ActionKind.Urbanization) Player1 game0
let ng0 = game0
let ng1 = increase IncreasableItem.AvailableResource Player2 ng0
let ng2 = increase IncreasableItem.AvailableResource Player2 ng1

type CharacterCard = 
    { id : CharacterId
      color : BuildingColor
      group : CharacterGroup
      initialItems : InitialItem list
      onAction : OnAction }

let character1 : CharacterCard = 
    { id = 1
      color = Red
      group = Group1
      initialItems = [ InitialItem.Resource; InitialItem.SuccessPoint; InitialItem.BuildingTile ]
      onAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource) }

// --
// http://www.devx.com/dotnet/Article/40537/0/page/3#sthash.xM1pn761.dpuf
let randomNumberGenerator = new System.Random()

// type is specified to prevent crash on mono...
let shuffle (cards : Card list) = 
    let upperBound = (List.length cards) * 100
    let weightedCards = List.map (fun card -> card, randomNumberGenerator.Next(0, upperBound)) cards
    let sortedWeightedCards = 
        List.sortWith (fun (_, leftWeight) (_, rightWeight) -> leftWeight - rightWeight) weightedCards
    List.map (fun (card, _) -> card) sortedWeightedCards

let shuffleCards (cards : Card list) = shuffle cards

let initialCity = 
    [ for num in 1..3 do
          for color in [ Blue; Red; Yellow ] -> 
              BuildingCard { color = color
                             number = num } ]

printfn "Initial city layout: %A" initialCity
printfn "About to shuffle tiles"

let shuffled = shuffle initialCity

printfn "Tiles shuffled"
printfn "%A" shuffled
