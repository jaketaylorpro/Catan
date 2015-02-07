module YamlOut
open Microsoft.FSharp.Reflection
let toYaml (o:obj) :string =
    let rec toYamli (o) (ind:int) :string =
        let rec tabs n=
            match n with
            | 0 -> ""
            | _ -> "  "+(tabs (n-1))
        let arrayToYaml l (ind:int)=
            Array.fold (fun acc 
                            i
                                ->acc+(tabs ind)+"- "+((toYamli i (ind+1))+"\n"))
                        ""
                        l 
        let classToYaml c (ind:int) =
            let firstProperty=c.GetType().GetProperties()
                                |>Array.toList
                                |>List.head
            let restProperties=c.GetType().GetProperties()
                                |>Array.toList
                                |>List.tail
                                |>List.toArray
            firstProperty.Name+": "+
            (toYamli (firstProperty.GetMethod.Invoke(c,[]|>List.toArray)) 0)+
            "\n"+
            Array.fold (fun (acc:string)
                            (pi:System.Reflection.PropertyInfo) ->
                                acc+(tabs ind)+pi.Name+": "+(toYamli (pi.GetMethod.Invoke(c,[]|>List.toArray)) (ind+1))+"\n")
                       "" 
                       restProperties
        let unionToYaml (u:obj) (ind:int) =
            (*u.GetType().Name+"."+*)(sprintf "%A" u)
        match o with
        | a when a.GetType().IsArray -> arrayToYaml (a:?>array<_>) ind
        | u when FSharpType.IsUnion (u.GetType()) -> unionToYaml u ind
        //| t when FSharpType.IsTuple (t.GetType()) -> tupleToYaml t ind
        //| f when FSharpType.IsFunction (f.GetType()) -> functionToYaml f ind
        | p when p.GetType().IsPrimitive -> p.ToString()
        | c when c.GetType().IsClass -> classToYaml c ind
        //| v when v.GetType().IsValueType -> "(v)"+v.ToString()
        //| e when e.GetType().IsEnum -> "(e)"+e.ToString()
        | x -> "(x)"+x.ToString()
    toYamli o 0
