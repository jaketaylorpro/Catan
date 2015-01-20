module CatanRecordTypes
open Util
open CatanUnionTypes

type RollChit(roll:Roll,id:int) = 
    member this.Roll = roll
    member this.Id=id

type DevelopmentCard(developmentCardType:DevelopmentCardType,id:int,isNew:bool) =
    member this.DevelopmentCardType=developmentCardType
    member this.Id=id
    member this.IsNew=isNew

type Hex(terrain:Terrain,rollChit:Option<RollChit>,robber:Robber,id:int) =
    member this.Terrain=terrain
    member this.RollChit=rollChit
    member this.Robber=robber
    member this.Id=id

type HexNode (hex:Hex,hexNE:Option<HexNode>,hexE:Option<HexNode>,hexSE:Option<HexNode>,hexSW:Option<HexNode>,hexW:Option<HexNode>,hexNW:Option<HexNode>) =
    member this.Hex=hex
    member this.HexNorthEast=hexNE
    member this.HexEast=hexE
    member this.HexSouthEast=hexSE
    member this.HexSouthWest=hexSW
    member this.HexWest=hexW
    member this.HexNorthWest=hexNW

type Road(color:Color,id:int) = 
    member this.Color=color
    member this.Id=id
    interface System.IComparable with
        member this.CompareTo o = 
            match o with
            | :? Road as rd -> compare this.Id rd.Id 
            | _ -> 1
    override this.Equals o =
        match o with
        | :? Road as rd -> this.Id = rd.Id
        | _ -> false
    override this.GetHashCode() =
        hash(this.Id)
type Settlement(color:Color,id:int) =
    member this.Color=color
    member this.Id=id

type City(color:Color,id:int) =
    member this.Color=color
    member this.Id=id

type SettlementOrCity =    
    |ACity of City*Hex*Hex*Hex
    |ASettlement of Settlement*Hex*Hex*Hex
    member this.IsaCity = match this with
                            |ACity(_) -> true
                            |ASettlement(_) -> false
    member this.IsaSettlement = not this.IsaCity

type SettlementOrCityVertex(settlementOrCity:SettlementOrCity,hexLeft:Hex,hexRight:Hex,hexDown:Hex,harbor:Harbor) =
    member this.SettlementOrCity = settlementOrCity
    member this.HexLeft = hexLeft
    member this.HexRight = hexRight
    member this.HexDown = hexDown
    member this.Harbor = harbor
        
type RoadVertex (road:Road,HexUpLeft:Hex,HexDownRight:Hex) =
    member this.Road = road
    member this.HexUpLeft = HexUpLeft
    member this.HexDownRight = HexDownRight
    
type SettlementGraph =
    |DeadEnd
    |SettlementNode of Option<SettlementOrCityVertex> * Option<RoadVertex * SettlementGraph> * Option<RoadVertex * SettlementGraph> * Option<RoadVertex * SettlementGraph>

type Buildable = 
    |BuildableSettlement
    |BuildableCity
    |BuildableRoad
    |BuildableDevelopmentCard
        
type CatanMove = 
    |Build of (List<Resource>->List<Resource>*Buildable)
    |PlayDevelopmentCard of DevelopmentCard
    |TradeResources of (List<Resource>->List<Resource>)


let BuildCostMapping (b:Buildable) :List<Resource> =
    match b with
    |BuildableSettlement -> [Brick;Grain;Wood;Wool]
    |BuildableCity -> [Grain;Grain;Grain;Ore;Ore]
    |BuildableRoad -> [Brick;Wood]
    |BuildableDevelopmentCard -> [Grain;Ore;Wool]
