module XMLtagger
open System
open System.Text.RegularExpressions

(*
Koden her er lavet til at indsætte XML tags i F#.
den køres ved at lave et dll bibliotek og sætte den sammen med filen XMLrunner.fsx via fsharpc -r og kan herefter køres med mono.
det er ligegyldig hvor filen ligger på computeren den kan modtage en path til en mappe hvor filen ligger i.

by Anton -  mentorhold 2 
       *)
//basic XML insert
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
let Rlet = Regex "(\s)?(let)( )" //"^[\s]?(let)[ ]" 
let RXML = Regex "^[\s]?(\/\/XML)"
let Rmember = Regex "[\s]?(member)[ ]"
let Roverride = Regex "[\s]?(override)[ ]"
let Rtype = Regex "^[\s]?(type)[ ]"

///regex for navigation
let Rcomment = Regex "^(\s)?(\/\/)(.)?"   
let RXMLinPlace = Regex "^\/\/\/ ((<summary>)||(<param name =)||(<returns>))" 
let RlongCommentStart = Regex "\(\*"
let RlongComment = Regex "\*\)"

//til at finde antal \n til at genkende de rette let statements
let mutable NLcounter = 0

/// <summary> finder parametre </summary>
/// <param name = "ln" > Type string. linjen der skal undersøges </param>
/// <returns> string list - parametre. string returns parameteret  </returns>
let paramFinder (ln:string) =
    let mutable parStart = 0 // om der er en parantes der er startet
    let mutable equals = false // skal stoppe efter første = tegn
    let mutable newWord = "" // sidst genmte parameter
    let mutable returns = "" // den type der skal returneres
    let mutable paramLst = [] // liste af parametre
    let rec returnStart (i:int) = // finder typen som funktionen returnerer
        if i <= ln.Length - 1 then 
            match ln.[i] with
                |'=' -> ()
                |a ->
                    returns <- returns + string(a)
                    returnStart (i+1)
    for i = 0 to ln.Length - 1 do
      if not equals then
        match ln.[i] with
            |'(' when not equals -> // begunder at finde parameter ord
                parStart <- parStart + 1            //start gennemgane
                if parStart > 1 then
                    newWord <- newWord + "("
            |')' when not equals -> // parameter type slutter
                parStart <- parStart - 1
                if parStart = 0 then 
                    paramLst <- newWord :: paramLst
                    newWord <- ""
                else
                    newWord <- newWord + ")"
            |',' when not equals -> //sæt type ind og optag næste param
                paramLst <- newWord :: paramLst
                newWord <- ""
                if (paramLst.Length % 2) = 1 then // forskellen mellem et parameter navn eller nyt parameter
                    paramLst <- newWord :: paramLst
            |':' when not equals -> //slut tilføj og start hvis parStart = true
                if parStart > 0 then
                    paramLst <- newWord :: paramLst
                    newWord <- ""
                else
                    returnStart (i+1)
            |'=' -> // slutter med at gennemgå linjen
                equals <- true
                parStart <- 0
            |x when parStart > 0 ->  newWord <- newWord + (string(x))//hvis true så gem ellers skal den glemmes
            |_ -> ()
    paramLst <- List.rev (List.map (fun (x:string) -> x.Trim())paramLst )
    // listen af parametre vendes rundt så den komme i rigtige rækkefølge.
    returns <- returns.Trim()
    (paramLst, returns)



/// <summary> finder hvilken type tag der skal indsættes eller om der ikke skal indsættes </summary>
/// <param name = "ln" > Type string. linjen der skal undersøges </param>
/// <returns> Type string. det tag der skal indsættes </returns>
let TagCreator (ln:string) : string =
    let extracts = paramFinder ln
    let paramLst = fst(extracts)
    let returns = snd(extracts)
    let mutable tag = ""
    match ln with
        |a when (Rlet.IsMatch a && NLcounter >= 2) || Rmember.IsMatch a || Roverride.IsMatch a -> // \n\n let eller member eller override
            tag <- tag + summary
            if paramLst.Length > 1 then // hvis der er parameter navne
                for i in 0 .. 2 .. paramLst.Length - 2 do
                    tag <- tag + param1 + paramLst.[i] + param2 + paramLst.[i+1] + param3
            else tag <- tag
            if returns <> "" then //whis der er noget til returns
                tag <- tag + return1 + returns + return2
            else tag <- tag + return1Ln
        |a when Rtype.IsMatch a -> // type
            tag <- tag + summary
            if paramLst.Length > 1 then // hvis der er parameter navne
                for i in 0 .. 2 .. paramLst.Length - 2 do
                    tag <- tag + param1 + paramLst.[i] + param2 + paramLst.[i+1] + param3   
        |_ -> () // insæt ikke tag
    tag


/// <summary> gennemgår hver linje for at findlægge korekte XML tags </summary>
/// <param name = "oldfile" > Type string. nuværende fils navn </param>
/// <param name = "newName" > Type string. den nye fils navn </param>
/// <returns> Type unit. laver bare en ny fll </returns>
let ReadNWrite (oldfile:string)(newName:string) : unit =
    let mutable comment = false
    let mutable XMLInPlace = 0
    let NewFile = IO.File.CreateText newName
    let Stream = IO.File.OpenText oldfile
    while not Stream.EndOfStream do
        if XMLInPlace > 0 then // hvis der allerede er et tag skal den over se en linje
            XMLInPlace <- XMLInPlace - 1
        let ln = Stream.ReadLine ()
        match ln with
            |a when RlongCommentStart.IsMatch a -> //holder sig fra alt hvad der er udkommenteret
                NewFile.Write ( ln + "\n" )
                comment <- true
                if RlongCommentStart.IsMatch a && RlongComment.IsMatch a then comment <- false
                //til hvis noget er udkommenteret på en linje med (*   *)
            |a when RlongComment.IsMatch a -> // slutningen på en lang kommentar
                NewFile.Write (ln + "\n")
                comment <- false
            |a when a.Trim() = "" && not comment -> // genkender tomme linjer
                NewFile.Write "\n"
                NLcounter <- NLcounter + 1
            |a when RXML.IsMatch a && not comment -> // insdætter basic XML hvis der er //XML i filen
                NewFile.Write (Regex.Replace(ln, "[ ]?(//XML)", BasicXML) + "\n")
                NLcounter <- 0
             |a when RXMLinPlace.IsMatch a -> // genkender XML tags
                 NewFile.Write (ln + "\n")
                 XMLInPlace <- 2
                 NLcounter <- 0
            |a when Rcomment.IsMatch a && not comment -> // genkender andre kommentarer
                NewFile.Write (ln + "\n")
            |a when not comment && XMLInPlace = 0 ->
                // sender linjen videre for at se om der skal indsættes en kommentar
                NewFile.Write ((TagCreator ln) + ln + "\n" )
                NLcounter <- 0
            |a ->
                NewFile.Write (ln + "\n" ) //hvis der er en commentar eller lige har været et XML tag
    NewFile.Close()
    Stream.Close()
