namespace MusterLib


open System
open System.IO


module CSV =


    let parse (delim : char) (hasHdrs : bool) (lines : #seq<string>) : array<Map<string, string>> =
        let splitLine (line : string) = line.Split [|delim|] |> List.ofArray
        let rec parseLine (acc : list<string>) (acc2 : list<string>) (currLst : list<string>) : list<string> =
            match currLst, acc2 with
            | [], _ -> acc |> List.rev
            | h1 :: t1, _ when (h1.StartsWith @"""") && (h1.EndsWith @"""") ->
                parseLine ((h1.Replace(@"""", "")) :: acc) acc2 t1
            | h1 :: t1, [] when h1.Contains @"""" -> parseLine acc (h1 :: acc2) t1
            | h1 :: t1, _ :: _  when h1.Contains @"""" ->
                ((((String.Join(string delim, (h1 :: acc2) |> List.rev).Replace(@"""", "")).Trim()) :: acc), [], t1)
                |||> parseLine
            | h1 :: t1, _ :: _ -> parseLine acc (h1 :: acc2) t1
            | h1 :: t1, _ -> parseLine (h1.Trim() :: acc) acc2 t1
        if lines |> Seq.isEmpty then Array.empty
        else
            lines
            |> Seq.map (splitLine >> (parseLine [] []))
            |> (fun s ->
                let hdrs, dat =
                    if hasHdrs then Seq.head s, Seq.tail s
                    else [for i in 1 .. List.length(Seq.head s) -> "col" + (string i)], s
                dat |> Seq.map (fun t -> t |> List.zip hdrs |> Map.ofList)
            ) |> Array.ofSeq


    let parseFile (delim : char) (hasHdrs : bool) (filePath : string) : array<Map<string, string>> =
        filePath |> File.ReadAllLines |> parse delim hasHdrs

