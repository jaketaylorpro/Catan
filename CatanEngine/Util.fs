module Util
    module List =
        let rec firstSome (l:List<Option<'a>>) :Option<'a>= 
            match l with
            | [] -> None
            | Some(a)::t -> Some(a)
            | _ -> firstSome l.Tail
        let rec tailn<'a> (n:int) (l:List<'a>) :List<'a>=
            match n with
            |0 -> l
            |_ -> tailn (n-1) l
        let removen<'a> (n:int) (f:('a->bool)) (l:List<'a>) :List<'a>=
            l
            |> List.sortBy (fun x->if f x then 0 else 1)
            |> tailn n
        let zipn<'a> (l:List<List<'a>>) = 
            let rec zipnHelp (l:List<List<'a>>) (w:List<List<'a>>) =
                match l.Length with
                | 0 -> w
                | _ -> zipnHelp (List.tail l) (List.map2 (fun a b->a::b) (List.head l) w)
            zipnHelp l (List.map (fun e-> []) l)
        let removeIndex<'a> (l:List<'a>) (i:int) :List<'a> =
            l 
            |> List.mapi (fun (index:int) (o:'a) -> if index=i then [] else [o])
            |> List.collect (fun o->o)
    
        let rec containsm<'a when 'a : comparison> (sl:List<'a>) (l:List<'a>) :bool =
            match sl with
            |[] -> true
            |_ -> match List.tryFindIndex (fun o->o=sl.Head) l with
                   |None -> false
                   |Some(x) -> containsm (removeIndex l x) (sl.Tail)

        let rec removeAllm<'a when 'a : comparison> (sl:List<'a>) (l:List<'a>) :List<'a> =
            match sl with
            |[] -> l
            |_ -> let x= (List.findIndex (fun o->o=sl.Head) l)
                  removeAllm (removeIndex l x) (sl.Tail)
        let randHeadTail (l:List<'a>) :'a*List<'a>=
            let rnd=System.Random()
            let i=rnd.Next l.Length
            let head=List.nth l i
            let tail= removeIndex l i
            head,tail
        //array
        let arrayHeadTail a=
            let l=a|>Array.toList
            let head=l|>List.head
            let tail=l|>List.tail|>List.toArray
            head,tail
        let mapHeadTail m=
            let l=m|>Map.toList
            let head=l|>List.head
            let tail=l|>List.tail            
            head,new Map<_,_>(tail)
        let arrayFoldi (folder:('b->'a->int->'b)) (state:'b) (arr:'a[])=
            let rec helper f s (a:'a[]) (i:int)=
                match a.Length with
                |0 -> s
                |_ -> 
                    let h,t=arrayHeadTail a
                    helper f (f s h i) t (i+1)
            helper folder state arr 0
        let mapFoldi (folder:('b->'k->'v->int->'b)) (state:'b) (map:Map<'k,'v>)=
            let rec helper f s (m:Map<'k,'v>) (i:int)=
                match m.Count with
                |0 -> s
                |_ -> 
                    let (hk,hv),t=mapHeadTail m
                    helper f (f s hk hv i) t (i+1)
            helper folder state map 0
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