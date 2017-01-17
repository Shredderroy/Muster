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


    let isExtensionOf<'A when 'A : equality> (lst1 : list<'A>) (lst2 : list<'A>) : bool =
        let rec helper (currLst1 : list<'A>) (currLst2 : list<'A>) : bool =
            match currLst1, currLst2 with
            | h1 :: _, [] -> false
            | [], _ -> true
            | h1 :: t1, h2 :: t2 -> (h1 = h2) && (helper t1 t2)
        helper lst1 lst2


    let riffle (num : int) (elm : 'A) (lst : list<'A>) : list<'A> =
        lst
        |> Seq.unfold (function
            | h :: [] -> Some(seq [h], [])
            | h :: t -> Some(seq {yield h; yield! List.replicate num elm}, t)
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
                [acc; (List.init (len - accLen) (fun s -> minVal + (rnd.Next() % diffSucc)))]
                |> List.concat |> Seq.distinct |> List.ofSeq |> helper
        helper []

