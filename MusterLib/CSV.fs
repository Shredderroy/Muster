namespace MusterLib


open System
open System.IO


module CSV =


    let parse (delim : char) (hasHdrs : bool) (lines : #seq<string>) : array<Map<string, string>> =
        let splitLine (line : string) = line.Split [|delim|] |> List.ofArray
        let q = @""""
        let f (lst : list<string>) = lst |> (List.rev >> (String.concat (string delim)))
        let parseLine (lst : list<string>) : list<string> =
            (([], []), lst)
            ||> List.fold (fun (s, t) u ->
                let v = u.TrimStart()
                if (v.StartsWith q) && (v.EndsWith q) then
                    let w = v.Substring(1, max 0 (v.Length - 2))
                    match s with [] -> ([], w :: t) | _ -> ([], ((w :: s) |> f) :: t)
                elif (v.StartsWith q) then ((v.Substring 1) :: s, t)
                elif (v.EndsWith q) then ([], ((u.Substring(0, max 0 (v.Length - 2)) :: s) |> f) :: t)
                else match s with [] -> [], (u.Trim()) :: t | _ -> u :: s, t
            ) |> (function s, [] -> s | _, s -> s)
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

