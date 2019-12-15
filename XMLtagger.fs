module XMLtagger
open System
open System.Text.RegularExpressions


//basic insert
let BasicXML = """/// <summary>  </summary>
/// <param name = "">  </param>
/// <returns>  </returns>"""

/////custom inserts
//summary line
let summary = """/// <summary>  </summary>
"""

//param build
let param1 = "/// <param name = \""
let param2 = "\" > Type "
let param3 = ".  </param>
"

//returns build
let return1Ln = "/// <returns>  </returns>
"
let return1 = "/// <returns> Type "
let return2 = ".  </returns>
"

//////Regex
//regex for tag 
let Rlet = Regex "[\s]?(let)[ ]" 
let RXML = Regex "^[\s]?[//XML]"
let Rmember = Regex "[\s]?(member)[ ]"
let Rtype = Regex "^[\s]?(type)[ ]"

///regex for navigation
let Rcomment = Regex "^(\s)//.?"   
let RXMLinPlace = Regex "^(\s)?(///)(\s)?(<summary> || <param name || <returns>)"


let mutable NLcounter = 0

let paramFinder (ln:string) =
    let mutable parStart = 0
    let mutable newWord = ""
    let mutable returns = ""
    let mutable paramLst = []
    let rec returnStart (i:int) =
        if i <= ln.Length - 1 then
            match ln.[i] with
                |'=' -> ()
                |a ->
                    returns <- returns + string(a)
                    returnStart (i+1)
    for i = 0 to ln.Length - 1 do
        match ln.[i] with
            |'(' ->
                parStart <- parStart + 1            //start gennemgane
                if parStart > 1 then
                    newWord <- newWord + "("
            |')' ->
                parStart <- parStart - 1
                if parStart = 0 then 
                    paramLst <- newWord :: paramLst
                    newWord <- ""
                else
                    newWord <- newWord + ")"
            |',' -> //sæt type ind og optag næste param
                paramLst <- newWord :: paramLst
                newWord <- ""
            |':' -> //slut tilføj og start hvis parStart = true
                if parStart > 0 then
                    paramLst <- newWord :: paramLst
                    newWord <- ""
                else
                    returnStart (i+1)
            |'=' -> parStart <- 0
            |x when parStart > 0 ->  newWord <- newWord + (string(x))//hvis true så gem ellers skal den glemmes
            |_ -> ()
    paramLst <- List.rev (List.map (fun (x:string) -> x.Trim())paramLst )
    returns <- returns.Trim()
    (paramLst, returns)



let TagCreator (ln:string) : string =
    let extracts = paramFinder ln
    let paramLst = fst(extracts)
    let returns = snd(extracts)
    let mutable tag = ""
    match ln with
        |a when (Rlet.IsMatch a && NLcounter >= 2) || Rmember.IsMatch a -> 
            tag <- tag + summary
            if paramLst.Length > 1 then
                for i in 0 .. 2 .. paramLst.Length - 2 do
                    tag <- tag + param1 + paramLst.[i] + param2 + paramLst.[i+1] + param3
            else tag <- tag
            if returns <> "" then
                tag <- tag + return1 + returns + return2
            else tag <- tag + return1Ln
        |a when Rtype.IsMatch a -> 
            tag <- tag + summary
            if paramLst.Length > 1 then
                for i in 0 .. 2 .. paramLst.Length - 2 do
                    tag <- tag + param1 + paramLst.[i] + param2 + paramLst.[i+1] + param3   
        |_ -> ()
    tag
            

let ReadNWrite (oldfile:string)(newName:string) : unit =
    let NewFile = IO.File.CreateText newName
    let Stream = IO.File.OpenText oldfile
    while not Stream.EndOfStream do
        let ln = Stream.ReadLine ()
        match ln with
            |a when ln.Trim() = "" ->
                NewFile.Write "\n"
                NLcounter <- NLcounter + 1
            |a when RXML.IsMatch a ->
                NewFile.Write (Regex.Replace(ln, "[ ]?(//XML)", BasicXML) + "\n")
                NLcounter <- 0
            // |a when RXMLinPlace.IsMatch a ->
            //     NewFile.Write (ln + "\n")
            //     NLcounter <- 0
            // |a when Rcomment.IsMatch a ->
            //     NewFile.Write (ln + "\n")
            //     NLcounter <- NLcounter
            |a ->
                NewFile.Write ((TagCreator ln) + ln + "\n" )
                NLcounter <- 0
    NewFile.Close()
    Stream.Close()


    



