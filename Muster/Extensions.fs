namespace Muster.Extensions


open System
open System.Text.RegularExpressions


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


    let getTokens (str : string) =
        [for s in str.Split([|' '|]) do let t = s.Trim() in if not(String.IsNullOrEmpty t) then yield t]


    let getNGrams (str : string) : list<string> =
        let f = (fun s t -> [t] @ [for u in s -> t + " " + u] @ s)
        List.fold f [] (List.rev (getTokens str))


    let getShingles (str : string) : list<string> =
        let f = (fun s (i, t) -> [t] @ [for u in (Seq.take i s) -> t + " " + u] @ s)
        List.fold f [] (let toks = getTokens str in List.zip [0 .. (List.length toks) - 1] (List.rev toks))


    let getMaximalItems (lst : list<string>) : list<string> =
        let f = (fun s t -> (Seq.tryFind(fun (u : string) -> u.Contains t) s) |> Option.isSome)
        let g = (fun s t u -> not ((f s u) || (f t u)))
        let zLst = seq {for s in Seq.mapi (fun i s -> (i, s)) lst -> s}
        [for (s, t) in zLst do if g (Seq.take s lst) (Seq.skip (s + 1) lst) t then yield t]

