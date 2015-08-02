namespace Muster.DataStructuresAndAlgorithms


open System


module CART =


    [<RequireQualifiedAccess>]
    type CatType =
        | Int of int
        | Str of string


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type ContType =
        | Flt of float
        static member (+) (s, t) = match s, t with | ContType.Flt u, ContType.Flt v -> ContType.Flt (u + v)
        static member (-) (s, t) = match s, t with | ContType.Flt u, ContType.Flt v -> ContType.Flt (u - v)
        static member (*) (s, t) = match s, t with | ContType.Flt u, ContType.Flt v -> ContType.Flt (u * v)
        static member (/) (s, t) = match s, t with | ContType.Flt u, ContType.Flt v -> ContType.Flt (u / v)


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
        (subTblDat : list<CatType * CatType>)
        (impurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : list<float> =
        let subTblDatLen = float (List.length subTblDat)
        subTblDat
        |> Seq.groupBy (fst)
        |> Seq.map (fun (_, s) -> s |> Seq.map snd)
        |> Seq.map (fun s -> float (Seq.length s), ((impurityFunc << List.ofSeq) s))
        |> Seq.map (fun (s, t) -> s * t)
        |> Seq.sum
        |> (fun s -> [datSetImpurity - (s / subTblDatLen)])


    let getInfoGainForContVar
        (subTblDat : list<ContType * ContType>)
        (inpurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : list<float> =
        let sortedSubTblDat = List.sortBy (fst) subTblDat
        let subTblDatLen = float (List.length subTblDat)
        let res =
            ((snd << List.head) subTblDat, List.tail subTblDat)
            ||> List.scan (fun s t -> s + (snd t))
            |> List.tail
        []


    let test () : unit = ()

