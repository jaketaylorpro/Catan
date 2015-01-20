module CatanPlayer
open Util
open CatanUnionTypes
open CatanRecordTypes
//this represents settlements and roads in a specialized graph
type CatanPlayer(color:Color,resources:List<Resource>,developmentCards:List<DevelopmentCard>,specialCards:List<SpecialCard>,settlementGraph:SettlementGraph) = 
    member this.Color=color
    member this.Resources=resources
    member this.DevelopmentCards=developmentCards
    member this.SpecialCards=specialCards   
    member this.SettlementGraph=settlementGraph 
    member this.IterateRoads :List<Road>=
        []//TODO
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
                            |> List.filter (fun (h:Hex) -> (h.RollChit.IsSome && h.RollChit.Value.Roll = r))
                            |> List.map (fun(h:Hex) -> h.Terrain))
        |> List.map CatanUnionTypes.TerrainResourceMapping
        |> List.filter (fun (r:Option<Resource>)->r.IsSome)
        |> List.map (fun (r:Option<Resource>)->r.Value)
    member this.CalculateMoves :List<CatanMove>=
        let getTradeMovesForTradeRatioToOne (x:int) (nr:Resource) :List<CatanMove>=
            this.Resources
                |> Seq.groupBy (fun (r:Resource)->r)
                |> Map.ofSeq
                |> Map.map (fun k v -> Seq.length v)
                |> Map.filter (fun k v -> v>=x)
                |> Map.toList
                |> List.map (fun (k,v)->k)
                |> List.map (fun k->CatanMove.TradeResources(fun rcs->nr::(rcs
                                                                        |>Util.removen x (fun r->r=k))))//TODO doesn't support multiples yet
        let applyForEachResource f =
            [Brick;Wool;Ore;Grain;Wood]
            |>List.collect (fun r->f r)
        let buildMoves= [BuildableSettlement;BuildableCity;BuildableRoad;BuildableDevelopmentCard]
                        |> List.collect (fun b->if Util.containsm (BuildCostMapping b) this.Resources then [b] else [])
                        |> List.map (fun b->(fun r->(Util.removeAllm (BuildCostMapping b) r),b))
                        |> List.map (fun b->CatanMove.Build(b))//TODO doesn't support multiples yet
        let playMoves=this.DevelopmentCards
                        |> List.filter (fun dc->not dc.IsNew)
                        |> List.map (fun dc->CatanMove.PlayDevelopmentCard(dc))
        let tradeMoves=this.IterateSettlements
                        |> List.collect (fun (s:SettlementOrCityVertex) -> match s.Harbor with
                                                                            |Harbor.NoHarbor -> applyForEachResource (getTradeMovesForTradeRatioToOne 4)
                                                                            |Harbor.NormalHarbor -> applyForEachResource (getTradeMovesForTradeRatioToOne 3) 
                                                                            |Harbor.SpecialHarbor(x) -> applyForEachResource (getTradeMovesForTradeRatioToOne 2))
        List.concat [buildMoves;playMoves;tradeMoves]
    member this.CalculateScore :int= 
        let devCardPoints=
            this.DevelopmentCards 
            |> List.filter (fun dc -> dc.DevelopmentCardType = DevelopmentCardType.VictoryPoint) 
            |> List.length
        let specialCardPoints=this.SpecialCards.Length*2
        let settlementPoints=
            this.IterateSettlements
            |>List.sumBy (fun (soc:SettlementOrCityVertex)-> if soc.SettlementOrCity.IsaCity then 2 else 1)
        devCardPoints + specialCardPoints + settlementPoints