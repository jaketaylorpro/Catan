module YamlOut
open Microsoft.FSharp.Reflection
open Util
let toYaml (o:obj) :string =
    let rec tabs n=
            match n with
            | 0 -> ""
            | _ -> "  "+(tabs (n-1))
    let rec toYamli (o:obj) (ind:int) (inList:bool):string =
        //the strategy is to define any possible object in terms of strings, arrays, and maps
        let stringToYaml (s:string) (ind:int) (inl:bool) :string=
            s+"\n"
//            (tabs ind)+s+"\n"
        let arrayToYaml (l:'a[]) (ind:int) (inl:bool) :string=
            match l.Length with
            | 0 -> "[]\n"
            | _ ->
                let init=if inl then "" else "\n"
                Util.ArrayOps.arrayFoldi (fun acc v i-> acc+ (if inl && i=1 then "" else (tabs ind))+"- "+(toYamli v (ind+1) true)) init l
        let mapToYaml (m:Map<string,_>) (ind:int) (inl:bool) :string=
            match m.Count with
            | 0 -> "{}\n"
            | _ ->
                let init=if inl then "" else "\n"
                Util.MapOps.mapFoldi (fun acc k v i -> acc+(if inl && i=0 then "" else (tabs ind))+k+": "+(toYamli v (ind+1) false)) init m
        //handle record types
        let classToMap (c:'a) :Map<string,obj>=
            let p=c.GetType().GetProperties()
            Array.fold (fun acc
                            (pi:System.Reflection.PropertyInfo) ->
                                acc.Add(pi.Name,(pi.GetMethod.Invoke(c,[||]))))
                       (new Map<string,obj>([]))
                       p
        //handle union types
        let unionToMap (u:obj) :Map<string,_>=
            let info,fields=FSharpValue.GetUnionFields(u,(u.GetType()))
            new Map<string,_>([info.Name,fields])
        match o with
        | null -> stringToYaml "None" ind inList //None gets cast to null when its an obj
        | p when p.GetType().IsPrimitive -> stringToYaml (p.ToString()) ind inList
        | a when a.GetType().IsArray -> arrayToYaml (a:?>obj[]) ind inList
        | m when m.GetType().Name="Map"-> mapToYaml (m:?>Map<string,obj>) ind inList
        | u when Util.TypeOps.IsComplexUnionType (u)-> mapToYaml (unionToMap u) ind inList
        | u when Util.TypeOps.IsSimpleUnionType (u)-> stringToYaml (Printf.sprintf "%A" u) ind inList
        | c when c.GetType().IsClass -> mapToYaml (classToMap c) ind inList
        | x -> "(x)"+x.ToString()
    toYamli o 0 false
