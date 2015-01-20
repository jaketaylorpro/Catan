// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
let quit() = 
    System.Console.Write "press any key to quit"
    let q=System.Console.ReadLine()
    0 // return an integer exit code)
let printHelp argv =
    System.Console.WriteLine ("unknown command: "+(String.concat " " argv))
    quit()
let printValue argv =
    System.Console.WriteLine (Newtonsoft.Json.JsonConvert.SerializeObject argv)
    quit()
let buildAndPrint x =
    match x with 
    | "development-card-deck" -> printValue (CatanBoard.buildDevelopmentCardDeck)
    | _ -> printHelp ["build";x]

[<EntryPoint>]
let rec main argv :int= 
    System.Console.Write ">"
    let line=System.Console.ReadLine()
    match line.Split(' ')|>Array.toList with
    | [] | [""] -> (main argv)
    | ["build";x] -> buildAndPrint x
    | args -> printHelp args
