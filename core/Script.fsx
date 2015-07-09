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
// @see Railway programming http://fsharpforfunandprofit.com/rop/
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
      cards : BuildingCard list
      characters : CharacterId list
      tiles : Tile list }

type PlayerHand = Card list

type Game = 
    { playersToIds : Map<Player, PlayerId>
      playerStates : Map<PlayerId, PlayerState>
      playerHands : Map<PlayerId, PlayerHand>
      availableTiles : Tile list }

type GameError = 
    | TooMuchPlayer of Player list
    | UnknownPlayer of Player
    | PlayerIdNotBound of PlayerId
    | NoResourceAvailable
    | NoTileAvailableInGame
    | UnsupportedIncreasableItem

let resourcesAvailable (playerStateTT : TwoTrack<PlayerState, GameError>) : TwoTrack<int, GameError> = 
    match playerStateTT with
    | Error e -> Error e
    | Success ps -> Success ps.nbResourceAvailable

let playerStateFor (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) : TwoTrack<PlayerState, GameError> = 
    match gameTT with
    | Error e -> Error e
    | Success game -> 
        let { playerStates = ps; availableTiles = ts } : Game = game
        match ps.TryFind(player) with
        | None -> Error(PlayerIdNotBound player)
        | Some playerState -> Success playerState

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
    | EndGame = 10

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
            | _ -> Error UnsupportedIncreasableItem

let whenAction (requiredKind : ActionKind) (updater : UpdateGame) = 
    fun (kind : ActionKind) -> 
        if kind = requiredKind then updater
        else identityUpdateGame

let funChar1 = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
let ng = (funChar1 ActionKind.Urbanization) Player1 game0
let ng0 = game0
let ng1 = increase IncreasableItem.AvailableResource Player2 ng0
let ng2 = increase IncreasableItem.AvailableResource Player2 ng1

match (playerStateFor Player2 ng2 |> resourcesAvailable) with
| Error e -> Fail(sprintf "No error should have occured, got: %A" e)
| Success v -> AreEqual 2 v

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
let shuffle (cards : BuildingTile list) = 
    let upperBound = (List.length cards) * 100
    let weightedCards = List.map (fun card -> card, randomNumberGenerator.Next(0, upperBound)) cards
    let sortedWeightedCards = 
        List.sortWith (fun (_, leftWeight) (_, rightWeight) -> leftWeight - rightWeight) weightedCards
    List.map (fun (card, _) -> card) sortedWeightedCards

let shuffleCards (cards : BuildingTile list) = shuffle cards

let initialBuildingTiles : BuildingTile list = 
    [ for num in 1..3 do
          for color in [ Blue; Red; Yellow ] -> 
              { color = color
                number = num } ]

printfn "Initial city layout: %A" initialBuildingTiles
printfn "About to shuffle tiles"

let shuffled = shuffle initialBuildingTiles

printfn "Tiles shuffled"
printfn "%A" shuffled

type Coord = int * int

type TilesOrGreenSpace = 
    | BuildingTiles of BuildingTile list
    | GreenSpaceTile of GreenSpaceTile

type BuildingBlock = 
    { inConstruction : bool
      tiles : TilesOrGreenSpace
      player : Option<Player>
      nbResource : int }

type CityBlock = 
    | UrbanizationBlock of UrbanizationToken
    | BuildingBlock of BuildingBlock

type CityLayout = Map<Coord, CityBlock>

type City = 
    { layout : CityLayout }

let layoutCity (buildingTiles : BuildingTile list) = 
    let urbz = 
        [ ((-1, +2), UrbanizationBlock A)
          ((0, +2), UrbanizationBlock B)
          ((+1, +2), UrbanizationBlock C)
          ((+2, +1), UrbanizationBlock D)
          ((+2, 0), UrbanizationBlock E)
          ((+2, -1), UrbanizationBlock F)
          ((+1, -2), UrbanizationBlock G)
          ((0, -2), UrbanizationBlock H)
          ((-1, -2), UrbanizationBlock I)
          ((-2, -1), UrbanizationBlock J)
          ((-2, 0), UrbanizationBlock K)
          ((-2, +1), UrbanizationBlock L) ]
    
    let coords : Coord list = 
        [ for y in [ 1; 0; -1 ] do
              for x in [ -1; 0; 1 ] -> (x, y) ]
    
    let initBlock (tile : BuildingTile) : CityBlock = 
        BuildingBlock { inConstruction = false
                        tiles = BuildingTiles [ tile ]
                        player = Option.None
                        nbResource = 0 }
    
    let layoutBT = 
        buildingTiles
        |> List.map initBlock
        |> List.zip coords
        |> List.append urbz
        |> Map.ofList
    
    { layout = layoutBT }

let city = (layoutCity shuffled)

let keys = 
    city.layout
    |> Map.toList
    |> List.map (fun (k, v) -> k)

AreEqual 21 (List.length keys)

//
//         +-------+-------+-------+
//         |       |       |       |           
//         |   A   |   B   |   C   |           
//         |       |       |       |        
// +-------+-------+-------+-------+-------+    
// |       |       |       |       |       |   
// |   L   |       |       |       |   D   |   
// |       |       |       |       |       |
// +-------+-------+-------+-------+-------+    
// |       |       |       |       |       |   
// |   K   |       |   +   |       |   E   |   
// |       |       |       |       |       |
// +-------+-------+-------+-------+-------+    
// |       |       |       |       |       |   
// |   J   |       |       |       |   F   |   
// |       |       |       |       |       |
// +-------+-------+-------+-------+-------+    
//         |       |       |       |           
//         |   I   |   H   |   G   |           
//         |       |       |       |        
//         +-------+-------+-------+    
//
let rangeOf (cityLayout : CityLayout) : Coord * Coord = 
    cityLayout
    |> Map.toList
    |> List.map (fun (k, v) -> k)
    |> List.fold (fun ((xmin, ymin), (xmax, ymax)) (x, y) -> 
           let nxmin = 
               if x < xmin then x
               else xmin
           
           let nxmax = 
               if x > xmax then x
               else xmax
           
           let nymin = 
               if y < ymin then y
               else ymin
           
           let nymax = 
               if y > ymax then y
               else ymax
           
           ((nxmin, nymin), (nxmax, nymax))) ((+1, +1), (-1, -1))

AreEqual ((-2, -2), (+2, +2)) (rangeOf city.layout)

let printCity (city : City) = 
    let layout = city.layout
    let ((xmin, ymin), (xmax, ymax)) = rangeOf layout
    let cellHeight = 4
    
    let formatUrbz token = 
        [ ("       ")
          ("       ")
          (sprintf "  (%2A)  " token)
          ("       ") ]
    
    let formatBuildingTiles (b : BuildingBlock) (bs : BuildingTile list) = 
        let n = bs.Length
        [ (sprintf "%4A" bs.Head.color)
          (sprintf "%s %6A" (if b.inConstruction then "*"
                             else " ") bs.Head.number)
          (sprintf "%4s" (if b.player.IsSome then b.player.Value
                          else ""))
          (sprintf "#%d    " n) ]
    
    let formatGreenSpace (b : BuildingBlock) = 
        [ ("       ")
          ("   GS  ")
          (sprintf " (%2A)  " (if b.player.IsSome then b.player.Value
                               else ""))
          ("       ") ]
    
    let formatCellInfo (block : CityBlock) = 
        match block with
        | BuildingBlock b -> 
            match b.tiles with
            | BuildingTiles buildingTiles -> formatBuildingTiles b buildingTiles
            | GreenSpaceTile gs -> formatGreenSpace b
        | UrbanizationBlock token -> formatUrbz token
    
    let printCell (r : int) (y : int) (x : int) = 
        let cityBlockOpt : Option<CityBlock> = layout.TryFind(x, y)
        let exists = cityBlockOpt.IsSome
        let existsOrHasRightBlock = exists || layout.ContainsKey(x + 1, y)
        let existsOrHasBottomBlock = exists || layout.ContainsKey(x, y - 1)
        
        let fmt = 
            match r with
            | 4 when existsOrHasBottomBlock -> "_______"
            | _ when exists -> List.nth (formatCellInfo cityBlockOpt.Value) r
            | _ -> "       "
        printf "%7s%s" fmt (if existsOrHasRightBlock then "|"
                            else " ")
    
    let printRow (y : int) = 
        [ 0..cellHeight ] |> List.iter (fun r -> 
                                 [ xmin..xmax ] |> List.iter (printCell r y)
                                 printfn "")
    
    (List.rev [ (ymin - 1)..(ymax + 1) ]) |> List.iter printRow

printCity city