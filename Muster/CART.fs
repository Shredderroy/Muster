namespace Muster.DataStructuresAndAlgorithms


open System


module CART =


    let coreImpurityFn (classVals : list<_>) : list<float> =
        let classValsLen = float (List.length classVals)
        classVals
        |> Seq.groupBy (id)
        |> Seq.map (fun (s, t) -> (float (Seq.length t)) / classValsLen)
        |> List.ofSeq


    let entropy (classVals : list<_>) : float =
        classVals
        |> coreImpurityFn
        |> List.map (fun s -> s * Math.Log(s, 2.0))
        |> List.sum
        |> (*) (-1.0)


    let giniIndex (classVals : list<_>) : float =
        classVals
        |> coreImpurityFn
        |> List.map (fun s -> s * s)
        |> List.sum
        |> (-) 1.0


    let classificationError (classVals : list<_>) : float =
        classVals
        |> coreImpurityFn
        |> List.max
        |> (-) 1.0


//    let getInfoGainForCatVar (subTblDat : list<list<_>>) (impurityFn : (list<_> -> float)) (datSetImpurity : float) =
//        let subTblDatLen = List.length subTblDat


    let test () : unit = ()

