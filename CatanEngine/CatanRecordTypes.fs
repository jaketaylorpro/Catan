namespace CatanEngine
open Util
open CatanEngine

type RollChit(roll:Roll) = 
    member this.Roll = roll

type Hex(terrain:Terrain,rollChit:RollChit,hasRobber:bool) =
    member this.Terrain=terrain
    member this.Roll=rollChit
    member this.HasRobber=hasRobber

type Road(color:Color) = 
    member this.Color=color

//type Settlement(color:Color) =
    //member this.Color=color

//type City(color:Color) =
    //member this.Color=color

type SettlementOrCity =
    |City of Color
    |Settlement of Color

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
        let settlementPoints=
            this.Settlements
            |>List.sumBy (fun e->
                match e with
                |None -> 0
                |Some(s) -> match s with
                                |Settlement(_) -> 1 
                                |City(_) -> 2)
        devCardPoints + specialCardPoints + settlementPoints
    
    
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
