module YamlOut
open Microsoft.FSharp.Reflection
open Util
let toYaml (o:obj) :string =
    let rec tabs n=
            match n with
            | 0 -> ""
            | _ -> "  "+(tabs (n-1))
    let rec toYamli (o:obj) (ind:int) (inList:bool) (hist:Set<string*int>) :string =
        //the strategy is to define any possible object in terms of strings, arrays, and maps
        let stringToYaml (s:string) (ind:int) :string=
            s+"\n"
        let arrayToYaml (l:'a[]) (ind:int) :string=
            match l.Length with
            | 0 -> "[]\n"
            | _ ->
                let init=if inList then "" else "\n"
                Util.ArrayOps.arrayFoldi (fun acc v i-> acc+ (if inList && i=1 then "" else (tabs ind))+"- "+(toYamli v (ind+1) true hist)) init l
        let mapToYaml (m:Map<string,obj>) (ind:int) :string=
            let nhist,ref=match m.TryFind "Id" with
                            | None -> hist,None
                            | Some(x) when hist.Contains (x:?>string*int)-> hist,Some(x:?>string*int)
                            | Some(x) -> hist.Add (x:?>string*int),None
            match ref with
            | Some(name,id) -> "*"+name+id.ToString()
            | None ->
                match m.Count with
                | 0 -> "{}\n"
                | _ ->
                    let init=if inList then "" else "\n"
                    Util.MapOps.mapFoldi (fun acc k v i -> acc+(if inList && i=0 then "" else (tabs ind))+k+": "+(toYamli v (ind+1) false hist)) init m
        //handle record types
        let classToMap (c:'a) :Map<string,obj>=
            let p=c.GetType().GetProperties()
            Array.fold (fun acc
                            (pi:System.Reflection.PropertyInfo) ->
                                acc.Add(pi.Name,(pi.GetMethod.Invoke(c,[||]))))
                       (new Map<string,obj>([]))
                       p
        //handle union types
        let unionToMap (u:obj) :Map<string,obj>=
            let info,fields=FSharpValue.GetUnionFields(u,(u.GetType()))
            new Map<string,obj>([info.Name,fields:>obj])
        let tupleToArray (t:obj) :obj[]=
            FSharpValue.GetTupleFields(t)
        let t= if o=null then None else Some(o.GetType())
        let gt= if t.IsSome && t.Value.IsGenericType then Some(t.Value.GetGenericTypeDefinition()) else None
        match o with
        | null -> stringToYaml "None" ind //None gets cast to null when its an obj
        | :? seq<obj> as seq-> arrayToYaml (seq|>Seq.toArray) ind
        | :? string as str-> stringToYaml str ind
        | :? Map<string,obj> as map -> mapToYaml map ind
        | pr when t.Value.IsPrimitive -> stringToYaml (pr.ToString()) ind
        | arr when t.Value.IsArray -> arrayToYaml (arr:?>obj[]) ind
        | u when Util.TypeOps.IsComplexUnionType (u)-> mapToYaml (unionToMap u) ind
        | u when Util.TypeOps.IsSimpleUnionType (u)-> stringToYaml (Printf.sprintf "%A" u) ind
        | tup when FSharpType.IsTuple(t.Value) -> arrayToYaml (tupleToArray tup) ind
        | c when t.Value.IsClass -> mapToYaml (classToMap c) ind
        | x -> "(x)"+x.ToString()
    toYamli o 0 false (set[])
