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

type Hex(terrain:Terrain,rollChit:RollChit,hasRobber:bool,id:int) =
    member this.Terrain=terrain
    member this.RollChit=rollChit
    member this.HasRobber=hasRobber
    member this.Id=id

type Road(color:Color,id:int) = 
    member this.Color=color
    member this.Id=id
    interface System.IComparable with
        member this.CompareTo o = 
            match o with
            | :? Road as rd -> compare this.Id rd.Id 
            | _ -> 1

type Settlement(color:Color,id:int) =
    member this.Color=color
    member this.Id=id

type City(color:Color,id:int) =
    member this.Color=color
    member this.Id=id

type SettlementOrCity =    
    |ACity of City*Hex*Hex*Hex
    |ASettlement of Settlement*Hex*Hex*Hex
    member this.IsACity = match this with
                            |ACity(_) -> true
                            |ASettlement(_) -> false
    member this.IsASettlement = not this.IsACity

type SettlementOrCityVertex(settlementOrCity:SettlementOrCity,hexLeft:Hex,hexRight:Hex,hexDown:Hex) =
    member this.SettlementOrCity = settlementOrCity
    member this.HexLeft = hexLeft
    member this.HexRight = hexRight
    member this.HexDown = hexDown
        
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

//this represents settlements and roads in a specialized graph
type CatanPlayer(color:Color,resources:List<Resource>,developmentCards:List<DevelopmentCard>,specialCards:List<SpecialCard>,settlementGraph:SettlementGraph) = 
    member this.Color=color
    member this.Resources=resources
    member this.DevelopmentCards=developmentCards
    member this.SpecialCards=specialCards   
    member this.SettlementGraph=settlementGraph 
    member this.IterateSettlements :List<SettlementOrCityVertex>=
        let rec IterateSettlementsHelper gr h =
            let IterateSettlementHelperIfSomeAndNotVisited oGr (h:Set<Road>)=
                match oGr with
                |None -> []
                |Some(r:RoadVertex,s:SettlementGraph) -> if (h.Contains r.Road) then [] else (IterateSettlementsHelper s h)
            match gr with
            |SettlementGraph.DeadEnd -> []
            |SettlementGraph.SettlementNode( s,r1,r2,r3 )-> 
                let sList=if s.IsSome then [s] else []
                let r1List=IterateSettlementHelperIfSomeAndNotVisited r1 h
                let r2List=IterateSettlementHelperIfSomeAndNotVisited r2 h
                let r3List=IterateSettlementHelperIfSomeAndNotVisited r3 h
                List.append (List.append (List.append r1List r2List) r3List) sList
        IterateSettlementsHelper this.SettlementGraph (new Set<Road>([])) 
        |> List.filter (fun sg -> sg.IsSome)
        |> List.map (fun sg -> sg.Value)
    member this.CalculateResourcesFromRoll (r:Roll) :List<Resource> =
        this.IterateSettlements
        |> List.collect (fun (s:SettlementOrCityVertex) -> 
                            [s.HexLeft;s.HexRight;s.HexDown] 
                            |> List.filter (fun (h:Hex) -> h.RollChit.Roll = r)
                            |> List.map (fun(h:Hex) -> h.Terrain))
        |> List.map CatanUnionTypes.TerrainResourceMapping
        |> List.filter (fun (r:Option<Resource>)->r.IsSome)
        |> List.map (fun (r:Option<Resource>)->r.Value)
    member this.CalculateMoves :List<CatanMove>=
        let buildMoves= [BuildableSettlement;BuildableCity;BuildableRoad;BuildableDevelopmentCard]
                        |> List.collect (fun b->if Util.containsm this.Resources (BuildCostMapping b) then [b] else [])
                        |> List.map (fun b->(fun r->(Util.removeAllm r (BuildCostMapping b)),b))
                        |> List.map (fun b->CatanMove.Build(b))//TODO doesn't support multiples yet
        let playMoves=this.DevelopmentCards
                        |> List.filter (fun dc->not dc.IsNew)
                        |> List.map (fun dc->CatanMove.PlayDevelopmentCard(dc))
        let tradeMoves=[]//TODO
        List.concat [buildMoves;playMoves;tradeMoves]
    member this.CalculateScore :int= 
        let devCardPoints=
            this.DevelopmentCards 
            |> List.filter (fun dc -> dc = DevelopmentCard.VictoryPoint) 
            |> List.length
        let specialCardPoints=this.SpecialCards.Length*2
        let settlementPoints=
            this.IterateSettlements
            |>List.sumBy (fun (soc:SettlementOrCityVertex)-> if soc.SettlementOrCity.IsACity then 2 else 1)
        devCardPoints + specialCardPoints + settlementPoints

//this represents settlements and roads in a full heap
//type CatanPlayer(color:Color,resources:List<Resource>,developmentCards:List<DevelopmentCard>,specialCards:List<SpecialCard>,settlements:List<Option<SettlementOrCity>>,roads:List<Option<Road>>) = 
//    member this.Color=color
//    member this.Resources=resources
//    member this.DevelopmentCards=developmentCards
//    member this.SpecialCards=specialCards
//    member this.Settlements=settlements
//    member this.Roads=roads
//    member this.CalculateScore = 
//        let devCardPoints=
//            this.DevelopmentCards 
//            |> List.filter (fun dc -> dc = DevelopmentCard.VictoryPoint) 
//            |> List.length
//        let specialCardPoints=this.SpecialCards.Length*2
//        let settlementPoints=
//            this.Settlements
//            |>List.sumBy (fun e->
//                match e with
//                |None -> 0
//                |Some(s) -> match s with
//                                |Settlement(_) -> 1 
//                                |City(_) -> 2)
//        devCardPoints + specialCardPoints + settlementPoints
    
type CatanBoard(hexes:List<Hex>,ports:List<Harbor>,players:List<CatanPlayer>) =
    member this.Hexes= hexes
    member this.Ports=ports
    member this.Players=players
    member this.Settlements=
        players
        |>List.map (fun p->p.Settlements)
        |>Util.zipn
        |>List.map (fun s-> s |> List.find (fun e->e.IsSome))
    member this.Roads=
        players
        |>List.map (fun p->p.Roads)
        |>Util.zipn
        |>List.map (fun s-> s |> List.find (fun e->e.IsSome))
