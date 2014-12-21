namespace CatanEngine

type Terrain =  Hills | Pasture | Mountains | Fields | Forest | Desert | Water
type Resource = Brick | Wool    | Ore       | Grain  | Wood
type Port =
    | Some of Resource
    | Any
type Roll = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
type Color = Red | Blue | Orange | White
type DevelopmentCard = RoadBuilding | YearOfPlenty | Monopoly | Knight | VictoryPoint
type SpecialCard = LongestRoad | LargestArmy