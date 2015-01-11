module CatanBoard
open Util
open CatanUnionTypes
open CatanRecordTypes
open CatanPlayer
   
type CatanBoard(hexMap:HexNode,developmentCardDeck:List<DevelopmentCard>,players:List<CatanPlayer>) =
    member this.HexMap = hexMap
    member this.DevelopmentCardDeck = developmentCardDeck
    member this.Players = players
    member this.Settlements =
        players
        |>List.collect (fun p->p.IterateSettlements)
    member this.Roads =
        players
        |>List.collect (fun p->p.IterateRoads)

let buildDevelopmentCardDeck :List<DevelopmentCard>=
    //id ranges
    let kn=14   //14 knights
    let vp=kn+5 //5  vicoryPoint
    let mo=vp+2 //2  monopoly
    let rb=mo+2 //2  roadBuilding
    let yp=rb+2 //2  yearOfPlenty
    let rec r (l:List<DevelopmentCard>) (id:int) :List<DevelopmentCard> =
        let rhelp (t:DevelopmentCardType) :List<DevelopmentCard> =
            r (new DevelopmentCard(t,id,true)::l) (id+1)
        match id with
        | i when i<kn -> rhelp DevelopmentCardType.Knight
        | i when i<vp -> rhelp DevelopmentCardType.VictoryPoint
        | i when i<mo -> rhelp DevelopmentCardType.Monopoly
        | i when i<rb -> rhelp DevelopmentCardType.RoadBuilding
        | i when i<yp -> rhelp DevelopmentCardType.YearOfPlenty
        |_ -> l
    r [] 0

let buildTerrainDeck :List<Terrain> =
    //id ranges
    let fst=4    //4 forest
    let pst=fst+4//4 pasture
    let fld=pst+4//4 field
    let hls=fld+3//3 hills
    let mnt=hls+3//3 mountains
    let dst=mnt+1//1 desert
    let rec r (l:List<Terrain>) (id:int) :List<Terrain> =
        let rhelp (t:Terrain) :List<Terrain> =
            r (t::l) (id+1)
        match id with
        | i when i<fst -> rhelp Terrain.Forest
        | i when i<pst -> rhelp Terrain.Pasture
        | i when i<fld -> rhelp Terrain.Fields
        | i when i<hls -> rhelp Terrain.Hills
        | i when i<mnt -> rhelp Terrain.Mountains
        | i when i<dst -> rhelp Terrain.Desert
        |_ -> l
    r [] 0
let buildRollChitDeck :List<RollChit> =
    //id ranges
    let _2=1   //1 2
    let _3=_2+2//2 3s
    let _4=_3+2//2 4s
    let _5=_4+2//2 5s
    let _6=_5+2//2 6s
    let _8=_6+2//2 8s
    let _9=_8+2//2 9s
    let _10=_9+2//2 10s
    let _11=_10+2//2 11s
    let _12=_11+1//1 12s
    let rec r (l:List<RollChit>) (id:int) :List<RollChit> =
        let rhelp (t:Roll) :List<RollChit> =
            r (new RollChit(t,id)::l) (id+1)
        match id with
        | i when i<_2 -> rhelp Roll.Two
        | i when i<_3 -> rhelp Roll.Three
        | i when i<_4 -> rhelp Roll.Four
        | i when i<_5 -> rhelp Roll.Five
        | i when i<_6 -> rhelp Roll.Six
        | i when i<_8 -> rhelp Roll.Eight
        | i when i<_9 -> rhelp Roll.Nine
        | i when i<_10 -> rhelp Roll.Ten
        | i when i<_11 -> rhelp Roll.Eleven
        | i when i<_12 -> rhelp Roll.Twelve
        |_ -> l
    r [] 0
let buildHexMap :HexNode =
    let terrainDeck=buildTerrainDeck
    let rollChitDeck=buildRollChitDeck
    let rnd=System.Random()
    let rec r (h:Option<HexNode>)(td:List<Terrain>) (rcd:List<RollChit>) (id:int) :HexNode =
        let ti=rnd.Next td.Length
        let t=List.nth td ti
        let nextTerrainDeck= Util.removeIndex td ti
        let rci=rnd.Next rcd.Length
        let rc = if t=Terrain.Desert then None else Some(List.nth rcd rci)
        let nextRollChitDeck = if t=Terrain.Desert then rcd else Util.removeIndex rcd rci
        match h with
        | None -> r (Some(new HexNode(new Hex(t,rc,Robber.NoRobber,id),None,None,None,None,None,None))) nextTerrainDeck nextRollChitDeck (id+1) 
        | _ ->
            match td with
            | [] -> h.Value
            | _ -> r (h.Value.attach (new Hex(t,rc,Robber.NoRobber,id)) id) nextTerrainDeck nextRollChitDeck (id+1)
    r None terrainDeck rollChitDeck 0