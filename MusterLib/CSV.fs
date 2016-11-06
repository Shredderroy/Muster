namespace MusterLib


open System
open System.IO


module CSV =


    let parse (delim : char) (hasHdrs : bool) (strs : #seq<string>) : Map<int, Map<string, string>> =
        let splitLine (str : string) = str.Split [|delim|] |> Array.map (fun s -> s.Trim()) |> List.ofArray
        let rec parseLine (acc : list<string>) (acc2 : list<string>) (currLst : list<string>) : list<string> =
            match currLst, acc2 with
            | [], _ -> acc |> List.rev
            | h1 :: t1, [] when h1.Contains @"""" -> parseLine acc (h1 :: acc2) t1
            | h1 :: t1, _ :: _  when h1.Contains @"""" ->
                parseLine ((String.Join(string delim, (h1 :: acc2) |> List.rev).Replace(@"""", "")) :: acc) [] t1
            | h1 :: t1, _ :: _ -> parseLine acc (h1 :: acc2) t1
            | h1 :: t1, _ -> parseLine (h1 :: acc) acc2 t1
        if strs |> Seq.isEmpty then Map.empty
        else
            strs
            |> Seq.map (splitLine >> (parseLine [] []))
            |> (fun s ->
                let hdrs, dat =
                    if hasHdrs then Seq.head s, Seq.tail s
                    else [for i in 1 .. List.length(Seq.head s) -> "col" + (string i)], s
                dat |> Seq.mapi (fun i t -> i + 1, t |> List.zip hdrs |> Map.ofList)
            ) |> Map.ofSeq


    let parseFile (delim : char) (hasHdrs : bool) (filePath : string) : Map<int, Map<string, string>> =
        filePath |> File.ReadAllLines |> parse delim hasHdrs

