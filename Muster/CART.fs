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


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type DataType =
        | Cat of CatType
        | Cont of ContType


    type DataTable = list<array<DataType>>


    let inline coreImpurityFunc<'A when 'A : equality> (classVals : list<'A>) : list<float> =
        let classValsLen = float (List.length classVals)
        classVals
        |> Seq.groupBy (id)
        |> Seq.map (fun (s, t) -> (float (Seq.length t)) / classValsLen)
        |> List.ofSeq


    let entropy (classVals : list<DataType>) : float =
        classVals
        |> coreImpurityFunc
        |> List.map (fun s -> s * Math.Log(s, 2.0))
        |> (List.sum >> ((*) (-1.0)))


    let giniIndex (classVals : list<DataType>) : float =
        classVals
        |> coreImpurityFunc
        |> List.map (fun s -> s * s)
        |> (List.sum >> ((-) 1.0))


    let classificationError (classVals : list<DataType>) : float =
        classVals
        |> coreImpurityFunc
        |> (List.max >> ((-) 1.0))


    let getInfoGainForCatVar
        (tblDat : DataTable)
        (idx : int)
        (impurityFunc : (list<DataType> -> float))
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


    let emptyLstErrorMsg = "The given list is empty"
    
    
    let contErrorMsg = "Expected a continuous variable but encountered a categorical one"


    let applyContVarOp
        (lst : list<DataType>)
        (op : (list<float> -> 'A))
        : 'A =
        match lst with
        | [] -> failwith emptyLstErrorMsg
        | _ ->
            lst
            |> List.map (fun s ->
                match s with
                | DataType.Cont(ContType.Flt t) -> t
                | _ -> failwith contErrorMsg)
            |> op


    let getInfoGainForContVar
        (tblDat : DataTable)
        (idx : int)
        (impurityFunc : (list<DataType> -> float))
        (datSetImpurity : float)
        : array<float> =
        let sortedTblDat = tblDat |> List.sortBy (fun s -> s.[idx])
        let tblDatLen = float(List.length sortedTblDat)
        let rowLen = tblDat |> List.head |> Array.length
        ((List.head tblDat).[rowLen - 1], List.tail tblDat)
        ||> List.scan (fun s t ->
            applyContVarOp
                [s; t.[idx]]
                (fun (u : list<float>) -> DataType.Cont(ContType.Flt(List.reduce (+) u))))
        |> List.tail
        |> List.map (fun s ->
            sortedTblDat
            |> List.partition (fun t -> t.[idx] < s)
            |> (fun (t, u) -> [t; u])
            |> List.map (fun t ->
                (float(List.length t), impurityFunc(t |> List.map (fun u -> u.[rowLen - 1])))
                ||> (*))
            |> List.sum
            |> (fun t -> (applyContVarOp [s] (List.head)), datSetImpurity - (t / tblDatLen)))
        |> (fun s ->
            List.fold
                (fun t (u, v) -> if t.[0] > u then t else [|u; v|])
                (let (t, u) = List.head s in [|t; u|])
                (List.tail s))


    let getInfoGain
        (tblDat : DataTable)
        (idx : int)
        (impurityFunc : (list<_> -> float))
        (datSetImpurity : float)
        : array<float> =
        match (List.head tblDat).[idx] with
        | DataType.Cat(_) -> getInfoGainForCatVar tblDat idx impurityFunc datSetImpurity
        | _ -> getInfoGainForContVar tblDat idx impurityFunc datSetImpurity


    let getTblDatSplitsForCatVar (tblDat : DataTable) (idx : int) : list<DataTable> =
        tblDat
        |> Seq.groupBy (fun s -> s.[idx])
        |> Seq.map (snd >> List.ofSeq)
        |> List.ofSeq


    let getTblDatSplitsForContVar
        (tblDat : DataTable)
        (idx : int)
        (splittingValAndImpurity : array<float>)
        : list<DataTable> =
        tblDat
        |> List.sortBy (fun s -> s.[idx])
        |> List.partition (fun s ->
            match s.[idx] with
            | DataType.Cont(ContType.Flt v) -> v < splittingValAndImpurity.[0]
            | _ -> failwith contErrorMsg)
        |> (fun (s, t) -> [s; t])


    let test () : unit = ()

