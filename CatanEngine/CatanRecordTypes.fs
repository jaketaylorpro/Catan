namespace CatanEngine
open CatanEngine

type RollChit(roll:Roll) = 
    member this.Roll = roll

type Hex(terrain:Terrain,rollChit:RollChit,hasRobber:bool) =
    member this.Terrain=terrain
    member this.Roll=rollChit
    member this.HasRobber=hasRobber

type Road(color:Color) = 
    member this.Color=color

type SettlementOrCity(color:Color,isCity:bool) =
    member this.Color=color
    member this.IsCity = isCity
    member this.IsSettlement = not isCity

type CatanPlayer(color:Color,resources:List<Resource>,developmentCards:List<DevelopmentCard>,specialCards:List<SpecialCard>,settlements:List<Option<SettlementOrCity>>,roads:List<Option<Road>>) = 
    member this.Color=color
    member this.Resources=resources
    member this.DevelopmentCards=developmentCards
    member this.SpecialCards=specialCards
    member this.Settlements=settlements
    member this.Roads=roads
    member this.CalculateScore = 
        let devCardPoints=
            this.DevelopmentCards 
            |> List.filter (fun dc -> dc = DevelopmentCard.VictoryPoint) 
            |> List.length
        let specialCardPoints=this.SpecialCards.Length*2
        devCardPoints+specialCardPoints
    
type CatanBoard(hexes:List<Hex>,ports:List<Port>,players:List<CatanPlayer>) =
    member this.Hexes= hexes
    member this.Ports=ports
    member this.Players=players
    member this.Settlements=
        let listsOfSettlements=
            players
            |>List.collect (fun p->p.Settlements)
            |>Util.zipn
            |>list.collect (fun s-> List.find (fun e->e))
    member this.Roads=roads
