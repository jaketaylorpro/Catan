module YamlOut
open Microsoft.FSharp.Reflection
let toYaml (o:obj) :string =
    let rec toYamli (o) (ind:int) :string =
        let rec tabs n=
            match n with
            | 0 -> ""
            | _ -> "  "+(tabs (n-1))
        let arrayToYaml l (ind:int)=
            "(l)"+Array.fold (fun acc i->acc+(tabs ind)+"- "+(toYamli i (ind+1))+"\n") "\n" l 
        let recordToYaml r (ind:int) =
            "(r)"+Array.fold (fun (acc:string) (pi:System.Reflection.PropertyInfo)-> acc+(tabs ind)+pi.Name+": "+(toYamli (pi.GetMethod.Invoke(r,[]|>List.toArray)) (ind+1))+"\n") "\n" (FSharpType.GetRecordFields(r.GetType()))
        let classToYaml c (ind:int) =
            "(c)"+Array.fold (fun (acc:string) (pi:System.Reflection.PropertyInfo)-> acc+(tabs ind)+pi.Name+": "+(toYamli (pi.GetMethod.Invoke(c,[]|>List.toArray)) (ind+1))+"\n") "\n" (c.GetType().GetProperties())
        let unionToYaml (u:obj) (ind:int) =
            //(tabs ind)+"(u)"+(FSharpType.GetUnionCases(u.GetType())|>(Array.find (fun (ucase:UnionCaseInfo) -> ucase. = (Microsoft.FSharp.Core.LanguagePrimitives. u)))).Name
            //(tabs ind)+"(u)"+Array.fold (fun (acc:string) (ui:UnionCaseInfo)->acc+ui.Name+","+ui.Tag.ToString()+";") "" (FSharpType.GetUnionCases(u.GetType()))
            (tabs ind)+"(u)"+u.GetType().Name+"."+(sprintf "%A" u)
        let tupleToYaml t (ind:int) =
            (tabs ind)+"(t)"+t.ToString()
        let functionToYaml t (ind:int) =
            (tabs ind)+"(f)"+t.ToString()
        let t=o.GetType
        match o with
        | a when a.GetType().IsArray -> arrayToYaml (a:?>array<_>) ind
        | r when FSharpType.IsRecord (r.GetType()) -> recordToYaml r ind
        | u when FSharpType.IsUnion (u.GetType()) -> unionToYaml u ind
        | t when FSharpType.IsTuple (t.GetType()) -> tupleToYaml t ind
        | f when FSharpType.IsFunction (f.GetType()) -> functionToYaml f ind
        | p when p.GetType().IsPrimitive -> "(p)"+p.ToString()
        | c when c.GetType().IsClass -> classToYaml c ind
        | v when v.GetType().IsValueType -> "(v)"+v.ToString()
        | e when e.GetType().IsEnum -> "(e)"+e.ToString()
        | x -> "(x)"+x.ToString()
    toYamli o 0
