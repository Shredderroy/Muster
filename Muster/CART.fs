namespace Muster.DataStructuresAndAlgorithms


open System


module CART =


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type CatType =
        | Int of int
        | Str of string
        | Bool of bool


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type ContType =
        | Flt of float
        static member (+) (s, t) = match s, t with ContType.Flt u, ContType.Flt v -> ContType.Flt (u + v)
        static member (-) (s, t) = match s, t with ContType.Flt u, ContType.Flt v -> ContType.Flt (u - v)
        static member (*) (s, t) = match s, t with ContType.Flt u, ContType.Flt v -> ContType.Flt (u * v)
        static member (/) (s, t) = match s, t with ContType.Flt u, ContType.Flt v -> ContType.Flt (u / v)


    [<RequireQualifiedAccess>]
    type DataType =
        | Cat of CatType
        | Cont of ContType


    type DataTable = list<array<DataType>>


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
        (tblDat : DataTable)
        (idx : int)
        (impurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : array<float> =
        let tblDatLen = float(List.length tblDat)
        tblDat
        |> Seq.groupBy (fun s -> s.[idx])
        |> Seq.map (fun (_, s) ->
            s
            |> Seq.map (fun t -> t.[idx])
            |> (fun t -> float(Seq.length t), (impurityFunc << List.ofSeq) t)
            |> (fun (t, u) -> t * u))
        |> Seq.sum
        |> (fun s -> [|datSetImpurity - (s / tblDatLen)|])


    let getInfoGainForContVar
        (tblDat : DataTable)
        (idx : int)
        (impurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : array<float> =
        let sortedTblDat = tblDat |> List.sortBy (fun s -> s.[idx])
        let tblDatLen = float(List.length sortedTblDat)
        let rowLen = tblDat |> List.head |> Array.length
        let contErrorMsg idx =
            sprintf "Expected a continuous variable at position %d but encountered a categorical one" idx
        ((List.head tblDat).[rowLen - 1], List.tail tblDat)
        ||> List.scan (fun s t ->
            match s, t.[idx] with
            | DataType.Cont(ContType.Flt u), DataType.Cont(ContType.Flt v) ->
                DataType.Cont(ContType.Flt((u + v) / 2.0))
            | _ -> failwith(contErrorMsg idx))
        |> List.tail
        |> List.map (fun s ->
            sortedTblDat
            |> List.partition (fun t -> t.[idx] < s)
            |> (fun (t, u) -> [t; u])
            |> List.map (fun t ->
                (
                float(List.length t),
                impurityFunc(
                    t
                    |> List.map (fun u ->
                        match u.[idx] with
                        | DataType.Cont(ContType.Flt v) -> v
                        | _ -> failwith(contErrorMsg idx))))
                ||> (*))
            |> List.sum
            |> (fun t ->
                (match s with DataType.Cont(ContType.Flt u) -> u | _ -> failwith(contErrorMsg(rowLen - 1))),
                datSetImpurity - (t / tblDatLen)))
        |> (fun s ->
            List.fold
                (fun t (u, v) -> if t.[0] > u then t else [|u; v|])
                (let h = List.head s in [|fst h; snd h|])
                (List.tail s))


    let getInfoGain
        (subTbl : DataTable)
        (impurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : array<float> =
        [||]


    let test () : unit = ()

