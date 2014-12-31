module CatanUnionTypes

type Terrain =  Hills | Pasture | Mountains | Fields | Forest | Desert | Water
        
type Resource = Brick | Wool    | Ore       | Grain  | Wood



type Harbor =
    | SpecialHarbor of Resource
    | NormalHarbor
type Roll = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
type Color = Red | Blue | Orange | White
type DevelopmentCardType = RoadBuilding | YearOfPlenty | Monopoly | Knight | VictoryPoint
type SpecialCard = LongestRoad | LargestArmy

let TerrainResourceMapping (t:Terrain) :Option<Resource> =
    match t with
    |Hills      -> Some(Brick)
    |Pasture    -> Some(Wool)
    |Mountains  -> Some(Ore)
    |Fields     -> Some(Grain)
    |Forest     -> Some(Wood)
    |Desert|Water -> None
