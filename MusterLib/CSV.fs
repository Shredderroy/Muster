namespace MusterLib


open System
open System.IO


module CSV =


    let parse (delim : char) (hasHdrs : bool) (lines : #seq<string>) : array<Map<string, string>> =
        let splitLine (line : string) = line.Split [|delim|] |> List.ofArray
        let parseLine (lst : list<string>) : list<string> =
            let q = @""""
            (([], []), lst)
            ||> List.fold (fun (s, t) u ->
                if not (u.EndsWith q) then (u :: s, t)
                elif not (u.StartsWith q) then ([], ((u :: s) |> List.rev |> String.concat (string delim)) :: t)
                else ([], u :: t)
            ) |> snd
        if lines |> Seq.isEmpty then Array.empty
        else
            lines |> Seq.map (splitLine >> parseLine)
            |> (fun s ->
                let hdrs, dat =
                    if hasHdrs then Seq.head s, Seq.tail s
                    else [for i in 1 .. List.length(Seq.head s) -> "col" + (string i)], s
                dat |> Seq.map ((List.zip hdrs) >> Map.ofList)
            ) |> Array.ofSeq


    let parseDefault (lines : #seq<string>) = parse ',' true lines


    let parseFile (delim : char) (hasHdrs : bool) (filePath : string) : array<Map<string, string>> =
        filePath |> File.ReadAllLines |> parse delim hasHdrs


    let parseFileDefault (filePath : string) = parseFile ',' true filePath

