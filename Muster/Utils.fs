namespace Muster.Utils


open System


module Misc =


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

