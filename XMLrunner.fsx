open XMLtagger
open System


let startString = "\nProgrammet indsætter de nødvendige XML tags ved genkendelse af følgende tegn:
- to tomme linjer og et \'let\'
- member
- type
- //XML
hvis der ikke er to linje skift inden en funktions let binding vil der ikke blive indsat XML tags

Din fil vil ikke blive overskrevet men kopieret med XML tags i.
Allerede indsatte XML tags burde ikke vlive overskrevet.

"


printfn "%s" startString

let rec indput (filename:string)=
    try
        let file = Environment.CurrentDirectory + "/" + filename
        let streamtest = IO.File.OpenText file
        streamtest.Close ()
        file
    with
        | :? IO.FileNotFoundException ->
            printfn "Den indtastede filnavn fandtes ikke prøv igen fx: ../Documents/storfil.fs\n"
            indput (Console.ReadLine ())
        | :? IO.DirectoryNotFoundException ->
            printfn "Den indtastede path var ikke valid prøv at skrive den igen. fx: ../Documents/storfil.fs"
            indput (Console.ReadLine ())
        |_ ->
            printfn "der skete en uventet fejl. prøv igen"
            indput (Console.ReadLine ())

printfn "Skriv: path fra denne mappe til fil /filename\n"
let changefile = indput (Console.ReadLine ())



let rec newFile (name:string) =
    let totalname = Environment.CurrentDirectory + "/" + name
    if totalname = changefile then
        printfn "det valgte navn må ikke være det samme som den den fil der skal indsættes i"
        newFile (Console.ReadLine ())
    else
        try
            if not (IO.File.Exists totalname) then
                ReadNWrite changefile totalname
                printfn "filen %A er nu oprettet" totalname
            else
                printfn "Den indtastede fil eksisterer allerede er du sikker på at du vil overskrive denne?\n Y/n?"
                let rec sureInput (n:int)=
                    let key = Console.ReadKey true
                    let kString = key.Key.ToString ()
                    let moder = key.Modifiers
                    match kString with
                        |"Y" |"Enter"->
                            ReadNWrite changefile totalname
                            printfn "filen %A er nu oprettet" totalname
                        |"N" ->
                            printfn "\nSkriv: path fra denne mappeog det nye filnavn fx. afleveringer/opg11g/filename.fsx\n"
                            newFile (Console.ReadLine ())
                        |_ ->
                            printfn "Ugyldig indtastning prøv igen Y/n?"
                            sureInput 1
                sureInput 1
                
        with
            | :? IO.DirectoryNotFoundException ->
                printfn "den valgte path fantes ikke prøv ingen\n"
                newFile (Console.ReadLine ())
            | _ ->
                printfn "Der skete desværre en uventet fejl, du kan prøve igen.. skriv path og filnavn til den fil som skal laves:\n"
                newFile (Console.ReadLine ())


 

printfn "\nSkriv: path fra denne mappe og det nye filnavn fx. afleveringer/opg11g/filename.fsx\n"
newFile (Console.ReadLine ())





