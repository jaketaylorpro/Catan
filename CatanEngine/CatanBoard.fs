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

let buildTerrainSudoDeck :List<Terrain> =
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
let buildHexDeck :List<Hex> =
    let terrainDeck=buildTerrainSudoDeck
    let rollChitDeck=buildRollChitDeck
    let rec r (l:List<Hex>)(td:List<Terrain>) (rcd:List<RollChit>) (id:int) :List<Hex> =
        match td with //we match on the terrain deck because it will be emptied last (it has one more element than the roll chit deck)
        |[] -> l
        |_ ->
            let t,nextTerrainDeck=Util.ListOps.randHeadTail td
            let rc,nextRollChitDeck,robber=match t with
                                            |Terrain.Desert -> None,rcd,Robber.Robber
                                            | _ -> let a,b=Util.ListOps.randHeadTail rcd
                                                   Some(a),b,Robber.NoRobber
            r (new Hex(t,rc,robber,id)::l) nextTerrainDeck nextRollChitDeck (id+1)
    r [] terrainDeck rollChitDeck 0




let getPath (oh:HexNode) (p:List<HexDirection>) :Option<HexNode> =
    let rec r (h:Option<HexNode>) (p:List<HexDirection>) :Option<HexNode> =
        match h with
        | None -> None
        | _ ->
            match p with
            | [] -> h
            | NorthEast::_  -> r h.Value.HexNorthEast p.Tail
            | East::_       -> r h.Value.HexEast p.Tail
            | SouthEast::_  -> r h.Value.HexSouthEast p.Tail
            | SouthWest::_  -> r h.Value.HexSouthWest p.Tail
            | West::_       -> r h.Value.HexWest p.Tail
            | NorthWest::_  -> r h.Value.HexNorthWest p.Tail
    r (Some(oh)) p

let copyAttach (oh:HexNode) (d:HexDirection) (h:HexNode) :HexNode=
    match d with
    | NorthEast -> new HexNode(oh.Hex, Some(h)           , oh.HexEast, oh.HexSouthEast, oh.HexSouthWest, oh.HexWest, oh.HexNorthWest)
    | East      -> new HexNode(oh.Hex, oh.HexNorthEast   , Some(h)   , oh.HexSouthEast, oh.HexSouthWest, oh.HexWest, oh.HexNorthWest)
    | SouthEast -> new HexNode(oh.Hex, oh.HexNorthEast   , oh.HexEast, Some(h)        , oh.HexSouthWest, oh.HexWest, oh.HexNorthWest)
    | SouthWest -> new HexNode(oh.Hex, oh.HexNorthEast   , oh.HexEast, oh.HexSouthEast, Some(h)        , oh.HexWest, oh.HexNorthWest)
    | West      -> new HexNode(oh.Hex, oh.HexNorthEast   , oh.HexEast, oh.HexSouthEast, oh.HexSouthWest, Some(h)   , oh.HexNorthWest)
    | NorthWest -> new HexNode(oh.Hex, oh.HexNorthEast   , oh.HexEast, oh.HexSouthEast, oh.HexSouthWest, oh.HexWest, Some(h)        )
let attach (oh:HexNode) (h:Hex) :HexNode =
        let attachd (d:HexDirection) (h:Hex) =
            match d with
            | HexDirection.NorthEast -> 
                let newNode=new HexNode(h,
                                        getPath oh [East;NorthEast;NorthWest],//NE
                                        getPath oh [East;NorthEast],//E
                                        getPath oh [East],//SE
                                        Some(oh),//SW
                                        getPath oh [NorthWest],//W
                                        getPath oh [NorthWest;NorthEast])//NW
                copyAttach oh d newNode
            | HexDirection.East ->
                let rec newNode=new HexNode(h,
                                        getPath oh [NorthEast;East],
                                        getPath oh [NorthEast;East;SouthEast],
                                        getPath oh [SouthEast;East],
                                        getPath oh [SouthEast],
                                        Some(oh),
                                        getPath oh [NorthEast])
                copyAttach oh d newNode
            | HexDirection.SouthEast -> 
                let rec newNode=new HexNode(h,
                                        getPath oh [East],
                                        getPath oh [East;SouthEast],
                                        getPath oh [East;SouthEast;SouthWest],
                                        getPath oh [SouthWest;SouthEast],
                                        getPath oh [SouthWest],
                                        Some(oh))
                copyAttach oh d newNode
            | HexDirection.SouthWest ->
                let rec newNode=new HexNode(h,
                                        Some(oh),
                                        getPath oh [SouthEast],
                                        getPath oh [SouthEast;SouthWest],
                                        getPath oh [SouthEast;SouthWest;West],
                                        getPath oh [West;SouthWest],
                                        getPath oh [West])
                copyAttach oh d newNode
            | HexDirection.West ->
                let rec newNode=new HexNode(h,
                                        getPath oh [NorthWest],
                                        Some(oh),
                                        getPath oh [SouthWest],
                                        getPath oh [SouthWest;West],
                                        getPath oh [SouthWest;West;NorthWest],
                                        getPath oh [NorthWest;West])
                copyAttach oh d newNode
            | HexDirection.NorthWest ->
                let rec newNode=new HexNode(h,
                                        getPath oh [NorthEast;NorthWest],
                                        getPath oh [NorthEast],
                                        Some(oh),
                                        getPath oh [West],
                                        getPath oh [West;NorthWest],
                                        getPath oh [West;NorthWest;NorthEast])
                copyAttach oh d newNode
        let row0=2
        let row1=4+row0
        let row2=5+row1
        let row3=4+row2
        let row4=3+row3
        match h.Id with
        | s,x when x<row0 -> attachd East h
        | s,x when x=row0 -> attachd SouthEast h
        | s,x when x<row1 -> attachd West h
        | s,x when x=row1 -> attachd SouthWest h
        | s,x when x<row2 -> attachd East h
        | s,x when x=row2 -> attachd SouthWest h
        | s,x when x<row3 -> attachd West h
        | s,x when x=row4 -> attachd SouthEast h
        | _             -> attachd East h

let buildHexMap :Option<HexNode> =
    let rec r (h:Option<HexNode>)(hd:List<Hex>) :Option<HexNode> =
        match hd.Length with
        | 0 -> h 
        | _ -> 
            let hex,nextHexDeck=Util.ListOps.randHeadTail hd
            match h with
            | None -> r (Some(new HexNode(hex,None,None,None,None,None,None))) nextHexDeck 
            | Some(hValue) ->
                r (Some(attach hValue hex)) nextHexDeck
    r None (buildHexDeck)
