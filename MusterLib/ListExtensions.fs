namespace MusterLib


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


    let isExtensionOf<'A when 'A : equality> (lst1 : list<'A>) (lst2 : list<'A>) =
        if (List.length lst2) < (List.length lst1) then false
        else
            (lst1, (lst2 |> List.take (List.length lst1)))
            ||> List.zip
            |> List.exists (fun (s, t) -> not(s = t))
            |> not


    let riffle (num : int) (elm : 'A) (lst : list<'A>) : list<'A> =
        lst
        |> Seq.unfold (function
            | h :: [] -> Some(seq [h], [])
            | h :: t -> Some(seq {yield h; yield! [for _ in 1..num -> elm]}, t)
            | _ -> None)
        |> Seq.collect id
        |> List.ofSeq


    let getDistinctRandomIntList (minVal : int) (maxVal : int) (len : int) : list<int> =
        let rnd = Random()
        let diffSucc = maxVal - minVal + 1
        let rec helper (acc : list<int>) : list<int> =
            let accLen = List.length acc
            if accLen >= len then acc |> Seq.skip (accLen - len) |> List.ofSeq
            else
                let newAcc =
                    [acc; (List.init (len - accLen) (fun s -> minVal + (rnd.Next() % diffSucc)))]
                    |> List.concat
                    |> Seq.distinct
                    |> List.ofSeq
                helper newAcc
        (helper []) |> List.ofSeq

