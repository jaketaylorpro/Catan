module YamlOut
open Microsoft.FSharp.Reflection
let toYaml (o:obj) :string =
    let rec toYamli (o:obj) (ind:int) :string =
        let rec tabs n=
            match n with
            | 0 -> ""
            | _ -> "  "+(tabs (n-1))
        let arrayToYaml l (ind:int)=
            Array.fold (fun acc i->acc+(tabs ind)+"- "+(toYamli i (ind+1))+"\n") "\n" l 
        let recordToYaml r (ind:int) =
            Array.fold (fun (acc:string) (pi:System.Reflection.PropertyInfo)-> acc+(tabs ind)+pi.Name+": "+(toYamli (pi.GetMethod.Invoke(r,[]|>List.toArray)) (ind+1))+"\n") "\n" (FSharpType.GetRecordFields(r.GetType()))
        let unionToYaml u (ind:int) =
            (tabs ind)+u.GetType.ToString()+"."+u.ToString()//handle parameterized unions SOME(x)
        let tupleToYaml t (ind:int) =
            (tabs ind)+t.ToString()
        let functionToYaml t (ind:int) =
            (tabs ind)+t.ToString()
        let t=o.GetType
        match o with
        | a when a.GetType().IsArray -> arrayToYaml (a:?>array<_>) ind
        | r when FSharpType.IsRecord (r.GetType()) -> recordToYaml r ind
        | u when FSharpType.IsUnion (u.GetType()) -> unionToYaml u ind
        | t when FSharpType.IsTuple (t.GetType()) -> tupleToYaml t ind
        | f when FSharpType.IsFunction (f.GetType()) -> functionToYaml f ind
        | x -> x.ToString()
    toYamli o 0
