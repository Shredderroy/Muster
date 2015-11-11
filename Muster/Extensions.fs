namespace Muster.Extensions


open System


module ListExtensions =


    let mapThread (f : list<'A> -> 'B) (lst : list<list<'A>>) : list<'B> =
        let rec helper (currLst : list<list<'A>>) (acc : list<'B>) : list<'B> =
            match (List.head currLst) with
            | [] -> acc |> List.rev
            | _ -> helper (currLst |> List.map List.tail) ((currLst |> List.map (List.head) |> f) :: acc)
        match lst with [] -> [] | _ -> helper lst []


    let transpose (lst : list<list<'A>>) : list<list<'A>> = mapThread id lst


    let pickFromList (lst : list<'A>) (idxLst : list<int>) : list<'A> =
        let rec helper
            (currLst : list<'A>)
            (currIdxLsti : list<int * int>)
            (lastIdx : int)
            (acc : list<int * 'A>)
            : list<int * 'A> =
            match currIdxLsti with
            | [] -> acc |> List.rev
            | (hdi, hd) :: tl ->
                let newCurrLst = currLst |> Seq.skip (hd - lastIdx - 1) |> List.ofSeq
                helper (List.tail newCurrLst) tl hd ((hdi, List.head newCurrLst) :: acc)
        (helper lst (idxLst |> List.mapi (fun i s -> i, s) |> List.sortBy snd) (-1) [])
        |> List.sortBy fst
        |> List.map snd


module StringExtensions =


    let getNonEmptyTokens (str : string) = [for s in str.Split([|' '|]) do let t = s.Trim() in if t <> "" then yield t]


    let getNGrams (str : string) : list<string> =
        List.fold (fun s t -> [t] @ [for u in s -> t + " " + u] @ s) [] ((List.rev << getNonEmptyTokens) str)


    let getShingles (str : string) : list<string> =
        (let toks = getNonEmptyTokens str in List.zip [0 .. (List.length toks) - 1] (List.rev toks))
        |> List.fold (fun s (i, t) -> [t] @ [for u in (Seq.take i s) -> t + " " + u] @ s) []

