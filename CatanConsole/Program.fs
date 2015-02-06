// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let quit() = 
    System.Console.Write "press any key to quit"
    let q=System.Console.ReadLine()
    0 // return an integer exit code)
let printHelp argv =
    System.Console.WriteLine ("unknown command: "+(String.concat " " argv))
let printValue argv =
    let serializedValue:string= YamlOut.toYaml argv
    System.Console.WriteLine serializedValue
let buildAndPrint x =
    match x with 
    | ["development-card-deck"] -> printValue (CatanBoard.buildDevelopmentCardDeck|>List.toArray)
    | _ -> printHelp ("build"::x)

[<EntryPoint>]
let rec main argv :int= 
    let h argv (f:(List<string>->unit)) (p:List<string>) =
        (f p)
        main argv
    System.Console.Write ">"
    let line=System.Console.ReadLine()
    match line.Split(' ')|>Array.toList with
    | [] | [""] -> (main argv)
    | ["build";x] -> (h argv (fun y->buildAndPrint y) [x])
    | ["quit"] -> quit()
    | args -> (h argv (fun y->printHelp y) args)
