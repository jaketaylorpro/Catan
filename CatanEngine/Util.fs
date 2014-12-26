module Util

let zipn<'a> l:List<List<'a>> = 
    let rec zipnHelp (l:List<List<'a>>) (w:List<List<'a>>) =
        match l.Length with
        | 0 -> w
        | _ -> zipnHelp (List.tail l) (List.map2 (fun a b->a::b) (List.head l) w)
    zipnHelp l (List.map (fun e-> []) l)

type HexGraph<'a ,'b when 'a : comparison and 'b : comparison>=
    | Vertex of 'a * Option<HexGraph<'a,'b>> * Option<HexGraph<'a,'b>> * Option<HexGraph<'a,'b>>
    | Edge of 'b * Option<HexGraph<'a,'b>> * Option<HexGraph<'a,'b>>
    member private this.countVerticiesOrEdges countV =
        let rec countVerticiesHelp (countV:bool) (n:HexGraph<'a,'b>) (h:Set<HexGraph<'a,'b>>) =
            let hn = h.Add n
            match n with
            | Vertex(v,e1,e2,e3) -> (if countV then 1 else 0) + 
                                    ([e1;e2;e3] 
                                        |> List.filter (fun e->e.IsSome)
                                        |> List.filter (fun e->not (h.Contains e.Value))
                                        |> List.sumBy (fun e->countVerticiesHelp countV e.Value hn))
            | Edge(e,v1,v2) -> (if countV then 0 else 1) + 
                                ([v1;v2]
                                    |> List.filter (fun v->v.IsSome)
                                    |> List.filter (fun v->not (h.Contains v.Value))
                                    |> List.sumBy (fun v->countVerticiesHelp countV v.Value hn))
        countVerticiesHelp countV this (new Set<HexGraph<'a,'b>>([]))
    member this.CountVerticies = this.countVerticiesOrEdges true
    member this.CountEdges = this.countVerticiesOrEdges false


