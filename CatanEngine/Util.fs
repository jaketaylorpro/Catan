module Util

let zipn<'a> l:List<List<'a>> = 
    let rec zipnHelp (l:List<List<'a>>) (w:List<List<'a>>) =
        match l.Length with
        | 0 -> w
        | _ -> zipnHelp (List.tail l) (List.map2 (fun a b->a::b) (List.head l) w)
    zipnHelp l (List.map (fun e-> []) l)
