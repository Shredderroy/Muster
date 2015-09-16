namespace Muster.Utils


open System


module Misc =


    let rnd = Random()


    let getDistinctRandomList (minVal : int) (maxVal : int) (len : int) : list<int> =
        let diffSucc = maxVal - minVal + 1
        let rec helper (acc : seq<int>) (remLen : int) : seq<int> =
            if remLen <= 0 then acc |> Seq.take len
            else
                helper
                    (
                    [acc; (Seq.init remLen (fun s -> rnd.Next() % diffSucc))]
                    |> Seq.concat
                    |> Seq.distinct)
                    (Seq.length acc)
        (helper Seq.empty len) |> List.ofSeq


    let test () : unit = ()

