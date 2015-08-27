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


    let catStrErrorMsg = "Expected a categorical variable of type string but encountered something else"
    
    
    let contErrorMsg = "Expected a continuous variable but encountered a categorical one"


    let defFltExtractorFn (sq : seq<DataType>) : seq<float> =
        sq
        |> Seq.map (fun s ->
            match s with
            | DataType.Cont(ContType.Flt t) -> t
            | _ -> failwith contErrorMsg)


    let applyExOp
        (exFn : seq<'A> -> seq<'B>)
        (op : seq<'B> -> 'C)
        (sq : seq<'A>)
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
            applyExOp defFltExtractorFn ((Seq.reduce (+)) >> ContType.Flt >> DataType.Cont) [s; t.[idx]])
        |> List.tail
        |> List.map (fun s ->
            sortedTblDat
            |> List.partition (fun t -> t.[idx] < s)
            |> (fun (t, u) -> [t; u])
            |> List.map (fun t -> float(List.length t) * impurityFunc(t |> List.map (fun u -> u.[rowLen - 1])))
            |> List.sum
            |> (fun t -> applyExOp defFltExtractorFn (Seq.head) [s], datSetImpurity - (t / tblDatLen)))
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


    let getTblDatSplitsForCatVar (tblDat : DataTable) (idx : int) _ : list<DataTable> =
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
        (applyExOp exFn op (tblDat |> List.sortBy (fun s -> s.[idx]))) |> List.ofSeq


    let getTblDatSplits
        (tblDat : DataTable)
        (idx : int)
        (splittingValAndImpurityOpt : option<array<float>>)
        : list<DataTable> =
        match (List.head tblDat).[idx] with
        | DataType.Cat _ -> getTblDatSplitsForCatVar tblDat idx None
        | DataType.Cont _ -> getTblDatSplitsForContVar tblDat idx splittingValAndImpurityOpt.Value


    let getPrunedComponentsForCatVar (tblsLst : list<DataTable>) (idx : int) _ _ : list<PrunedComponents> =
        let exFn (idx : int) (sqTbl: seq<seq<DataType>>) : seq<string * DataType * seq<seq<DataType>>> =
            if (Seq.isEmpty sqTbl) || (((Seq.skip idx) >> Seq.head >> Seq.length) sqTbl) < 2
            then failwith emptyLstErrorMsg
            else
                let colName, colVal =
                    let tmp = ((Seq.skip idx) >> Seq.head >> (Seq.take 2)) sqTbl in Seq.head tmp, Seq.last tmp
                match colName, colVal with
                | DataType.Cat(CatType.Str colNameStr), DataType.Cat _ -> seq [colNameStr, colVal, sqTbl]
                | _ -> failwith catErrorMsg
        let op (sq : seq<string * DataType * seq<seq<DataType>>>) : PrunedComponents =
            let colName, colVal, sqTbl = Seq.head sq
            {
            ColName = colName;
            ColVal = colVal;
            PrunedTable =
                (
                if idx < 1 then sqTbl |> Seq.skip 1
                else
                    seq {
                        yield! (Seq.take idx sqTbl)
                        yield! (Seq.skip(idx + 1) sqTbl)})
                |> ((Seq.map List.ofSeq) >> List.ofSeq)
                |> ListExtensions.transpose
                |> List.map (Array.ofList)}
        tblsLst
        |> List.map ((List.map List.ofArray) >> ListExtensions.transpose)
        |> List.map (Seq.map Seq.ofList)
        |> List.map (applyExOp (exFn idx) op)


    let epsilon = 0.001


    let defSplitStopCriterion (sqTbl : seq<seq<DataType>>) : bool = (Seq.length sqTbl) < 4


    let getPrunedComponentsForContVar
        (tblsLst : list<DataTable>)
        (idx : int)
        (splittingValAndImpurity : array<float>)
        (splitStopCriterion : seq<seq<DataType>> -> bool)
        : list<PrunedComponents> =
        let exFn (idx : int) (sqTbl : seq<seq<DataType>>) : seq<string * float * seq<seq<DataType>>> =
            if (Seq.isEmpty sqTbl) || (((Seq.skip idx) >> Seq.head >> Seq.length) sqTbl) < 2
            then failwith emptyLstErrorMsg
            else
                let colName, colVal =
                    let tmp = ((Seq.skip idx) >> Seq.head >> (Seq.take 2)) sqTbl in Seq.head tmp, Seq.last tmp
                match colName, colVal with
                | DataType.Cat(CatType.Str colNameStr), DataType.Cont(ContType.Flt v) -> seq [colNameStr, v, sqTbl]
                | _ -> failwith contErrorMsg
        let op (idx : int) (sq : seq<string * float * seq<seq<DataType>>>) : PrunedComponents =
            let colName, colVal, sqTbl = Seq.head sq
            {
            ColName = colName;
            ColVal =
                let sv = splittingValAndImpurity.[0]
                (if colVal < sv then sv - epsilon else sv) |> (DataType.Cont << ContType.Flt);
            PrunedTable =
                (
                if splitStopCriterion sqTbl then sqTbl
                elif idx < 1 then Seq.skip 1 sqTbl
                else
                    seq {
                        yield! (Seq.take idx sqTbl)
                        yield! (Seq.skip(idx + 1) sqTbl)})
                |> ((Seq.map List.ofSeq) >> List.ofSeq)
                |> ListExtensions.transpose
                |> List.map (Array.ofList)}
        tblsLst
        |> List.map ((List.map List.ofArray) >> ListExtensions.transpose)
        |> List.map (Seq.map Seq.ofList)
        |> List.map (applyExOp (exFn idx) (op idx))


    let getPrunedTbls
        (tblsLst : list<DataTable>)
        (idx : int)
        (splittingValAndImpurity : array<float>)
        (splitStopCriterion : seq<seq<DataType>> -> bool)
        : list<PrunedComponents> =
        let exFn (idx : int) (tblsSq : seq<DataTable>) : seq<bool * seq<DataTable>> =
            let colType = ((Seq.head << Seq.head) tblsSq).[idx]
            match colType with
            | DataType.Cat(CatType.Str _) -> seq [true, tblsSq]
            | _ -> failwith catErrorMsg
        let op (idx : int) (catFlgAndTblsSq : seq<bool * seq<DataTable>>) : list<PrunedComponents> =
            let catFlg, tblsSq = Seq.head catFlgAndTblsSq
            (if catFlg then getPrunedComponentsForCatVar else getPrunedComponentsForContVar)
                (List.ofSeq tblsSq)
                idx
                splittingValAndImpurity
                splitStopCriterion
        applyExOp (exFn idx) (op idx) tblsLst


    let buildC45
        (currTbl : DataTable)
        (impurityFn : list<DataType> -> float)
        (splitStopCriterion : seq<seq<DataType>> -> bool)
        =
        []


    let test () : unit = ()

