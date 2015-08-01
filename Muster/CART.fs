namespace Muster.DataStructuresAndAlgorithms


open System


module CART =


    [<RequireQualifiedAccess>]
    type CatType =
        | Int of int
        | Str of string


    [<RequireQualifiedAccess>]
    type ContType = | Flt of float


    [<RequireQualifiedAccess>]
    type DataType =
        | Cat of CatType
        | Cont of ContType


    type DataTable = list<list<DataType>>


    let coreImpurityFunc (classVals : list<_>) : list<float> =
        let classValsLen = float (List.length classVals)
        classVals
        |> Seq.groupBy (id)
        |> Seq.map (fun (s, t) -> (float (Seq.length t)) / classValsLen)
        |> List.ofSeq


    let entropy (classVals : list<_>) : float =
        classVals
        |> coreImpurityFunc
        |> List.map (fun s -> s * Math.Log(s, 2.0))
        |> (List.sum >> ((*) (-1.0)))


    let giniIndex (classVals : list<_>) : float =
        classVals
        |> coreImpurityFunc
        |> List.map (fun s -> s * s)
        |> (List.sum >> ((-) 1.0))


    let classificationError (classVals : list<_>) : float =
        classVals
        |> coreImpurityFunc
        |> (List.max >> ((-) 1.0))


    let getInfoGainForCatVar
        (subTbl : DataTable)
        (impurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : list<float> =
        let subTblDat = List.tail subTbl
        let subTblDatLen = float (List.length subTblDat)
        subTblDat
        |> Seq.groupBy (Seq.head)
        |> Seq.map (fun s -> s |> snd |> Seq.last)
        |> Seq.map (fun s -> (List.length s), (impurityFunc s))
        |> Seq.map (fun (s, t) -> (float s) * t)
        |> Seq.sum
        |> (fun s -> [datSetImpurity - (s / subTblDatLen)])


    let test () : unit = ()

