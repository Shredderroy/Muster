namespace Muster.DataStructuresAndAlgorithms


open System
open Muster.Extensions


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


    type PrunedComponents = {ColName : String; ColVal : DataType; PrunedTable : DataTable}


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


    let catErrorMsg = "Expected a categorical variable but encountered a continuous one"


    let catStrErrorMsg = "Expected a categorical variable of type string but encountered a different type"
    
    
    let contErrorMsg = "Expected a continuous variable but encountered a categorical one"


    let defFltExtractorFn (sq : seq<DataType>) : seq<float> =
        sq
        |> Seq.map (fun s ->
            match s with
            | DataType.Cont(ContType.Flt t) -> t
            | _ -> failwith contErrorMsg)


    let applyExOp
        (sq : seq<'A>)
        (exFn : seq<'A> -> seq<'B>)
        (op : seq<'B> -> 'C)
        : 'C =
        if Seq.isEmpty sq then failwith emptyLstErrorMsg
        else sq |> exFn |> op


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
            applyExOp
                [s; t.[idx]]
                defFltExtractorFn
                ((Seq.reduce (+)) >> ContType.Flt >> DataType.Cont))
        |> List.tail
        |> List.map (fun s ->
            sortedTblDat
            |> List.partition (fun t -> t.[idx] < s)
            |> (fun (t, u) -> [t; u])
            |> List.map (fun t ->
                (float(List.length t), impurityFunc(t |> List.map (fun u -> u.[rowLen - 1])))
                ||> (*))
            |> List.sum
            |> (fun t -> applyExOp [s] defFltExtractorFn (Seq.head), datSetImpurity - (t / tblDatLen)))
        |> (fun s ->
            List.fold
                (fun t (u, v) -> if t.[0] > u then t else [|u; v|])
                (let (t, u) = List.head s in [|t; u|])
                (List.tail s))


    let getInfoGain
        (tblDat : DataTable)
        (idx : int)
        (impurityFunc : (list<DataType> -> float))
        (datSetImpurity : float)
        : array<float> =
        match (List.head tblDat).[idx] with
        | DataType.Cat _ -> getInfoGainForCatVar tblDat idx impurityFunc datSetImpurity
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
        let exFn (sq : seq<array<DataType>>) : seq<bool * array<DataType>> =
            sq
            |> Seq.map (fun s ->
                match s.[idx] with
                | DataType.Cont(ContType.Flt v) -> (v < splittingValAndImpurity.[0]), s
                | _ -> failwith contErrorMsg)
        let op (sq : seq<bool * array<DataType>>) : seq<DataTable> =
            sq
            |> List.ofSeq
            |> List.partition (fst)
            |> (fun (s, t) -> [List.map (snd) s; List.map (snd) t] |> Seq.ofList)
        (applyExOp (tblDat |> List.sortBy (fun s -> s.[idx])) exFn op)
        |> List.ofSeq


    let getTblDatSplits
        (tblDat : DataTable)
        (idx : int)
        (splittingValAndImpurityOpt : option<array<float>>)
        : list<DataTable> =
        let errorMsg = "Incompatible DataType and splittingValAndImpurity"
        match (List.head tblDat).[idx], splittingValAndImpurityOpt with
        | DataType.Cat _, None -> getTblDatSplitsForCatVar tblDat idx
        | DataType.Cont _, Some splittingValAndImpurity -> getTblDatSplitsForContVar tblDat idx splittingValAndImpurity
        | _ -> failwith errorMsg


    let getPrunedTblForCatVar (tblLst : list<DataTable>) (idx : int) : list<PrunedComponents> =
        let res =
            let exFn (tblSq: seq<seq<DataType>>) : seq<string * DataType * seq<seq<DataType>>> =
                if (Seq.isEmpty tblSq) || (((Seq.skip idx) >> Seq.head >> Seq.length) tblSq) < 2
                then failwith emptyLstErrorMsg
                else
                    let colNameAndVal = ((Seq.skip idx) >> Seq.head >> (Seq.take 2)) tblSq
                    match Seq.head colNameAndVal, Seq.last colNameAndVal with
                    | DataType.Cat(CatType.Str colName), DataType.Cat _ -> seq [colName, Seq.last colNameAndVal, tblSq]
                    | _ -> failwith catErrorMsg
            let op (sq : seq<string * DataType * seq<seq<DataType>>>) : PrunedComponents =
                let colName, colVal, tblSq = Seq.head sq
                {ColName = colName; ColVal = colVal; PrunedTable = tblSq |> Seq.map (Array.ofSeq) |> List.ofSeq}
            tblLst
            |> List.map ((List.map List.ofArray) >> ListExtensions.transpose)
            |> List.map (fun s -> s
                )
        []


    let test () : unit = ()

