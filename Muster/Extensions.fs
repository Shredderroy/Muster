namespace Muster.Extensions


module ListExtensions =


    let mapThread (f : list<'A> -> 'B) (lst : list<list<'A>>) : list<'B> =
        let rec helper (currLst : list<list<'A>>) (currAcc : list<'B>) : list<'B> =
            match (List.head currLst) with
            | [] -> currAcc |> List.rev
            | _ -> helper (currLst |> List.map List.tail) ((currLst |> List.map (List.head) |> f) :: currAcc)
        match lst with [] -> [] | _ -> helper lst []


    let transpose (lst : list<list<'A>>) : list<list<'A>> = mapThread id lst


    let pickFromList (lst : list<'A>) (idxLst : list<int>) : list<'A> =
        let rec helper (currLst : list<'A>) (currIdxLst : list<int>) (acc : list<'A>) : list<'A> =
            match currIdxLst with
            | [] -> acc |> List.rev
            | hd :: tl -> 
        []

