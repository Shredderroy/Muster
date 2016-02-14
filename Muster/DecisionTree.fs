namespace Muster.DataStructuresAndAlgorithms


open System
open System.IO
open Muster.Extensions


module DecisionTree =


    let operatorErrorMsg (op : string) = op + " operator called with incompatible arguments"


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type CatType =
        | Int of int
        | Str of string
        | Bool of bool
        static member getDefaultOf s =
            match s with
            | CatType.Int _ -> CatType.Int 0
            | CatType.Str _ -> CatType.Str ""
            | CatType.Bool _ -> CatType.Bool true
        static member (+) (s, t) =
            match s, t with
            | CatType.Int u, CatType.Int v -> CatType.Int(u + v)
            | CatType.Str u, CatType.Str v -> CatType.Str(u + v)
            | CatType.Bool u, CatType.Bool v -> CatType.Bool(u || v)
            | _ -> failwith (operatorErrorMsg "+")
        static member (-) (s, t) =
            match s, t with
            | CatType.Int u, CatType.Int v -> CatType.Int(u - v)
            | _ -> failwith (operatorErrorMsg "-")
        static member (*) (s, t) =
            match s, t with
            | CatType.Int u, CatType.Int v -> CatType.Int(u * v)
            | CatType.Bool u, CatType.Bool v -> CatType.Bool(u && v)
            | _, CatType.Bool u -> if u then s else CatType.getDefaultOf s
            | CatType.Bool u, _ -> if u then s else CatType.getDefaultOf t
            | _ -> failwith (operatorErrorMsg "*")
        static member (/) (s, t) =
            match s, t with
            | CatType.Int u, CatType.Int v -> CatType.Int(u / v)
            | _ -> failwith (operatorErrorMsg "/")


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
        static member (+) (s, t) =
            match s, t with
            | DataType.Cat u, DataType.Cat v -> DataType.Cat(CatType.op_Addition(u, v))
            | DataType.Cont u, DataType.Cont v -> DataType.Cont(ContType.op_Addition(u, v))
            | _ -> failwith (operatorErrorMsg "+")
        static member (-) (s, t) =
            match s, t with
            | DataType.Cat u, DataType.Cat v -> DataType.Cat(CatType.op_Subtraction(u, v))
            | DataType.Cont u, DataType.Cont v -> DataType.Cont(ContType.op_Subtraction(u, v))
            | _ -> failwith (operatorErrorMsg "-")
        static member (*) (s, t) =
            match s, t with
            | DataType.Cat u, DataType.Cat v -> DataType.Cat(CatType.op_Multiply(u, v))
            | DataType.Cont u, DataType.Cont v -> DataType.Cont(ContType.op_Subtraction(u, v))
            | _ -> failwith (operatorErrorMsg "*")
        static member (/) (s, t) =
            match s, t with
            | DataType.Cat u, DataType.Cat v -> DataType.Cat(CatType.op_Division(u, v))
            | DataType.Cont u, DataType.Cont v -> DataType.Cont(ContType.op_Division(u, v))
            | _ -> failwith (operatorErrorMsg "/")


    type DataTable = list<array<DataType>>


    [<RequireQualifiedAccess>]
    type InfoGainRes = {SplittingValOpt : option<float>; InfoGain : float}


    [<RequireQualifiedAccess>]
    type ExcisedComponents = {ColName : String; ColVal : DataType; ExcisedTable : DataTable}


    [<RequireQualifiedAccess>]
    type Node =
        | Leaf of DataType
        | LeafList of list<DataType>
        | Internal of Map<DataType * DataType, Node>


    type ImpurityFn = list<DataType> -> float


    type SplitStopCriterion = seq<seq<DataType>> -> bool


    let errorMsgs =
        seq [
            ("emptyLstErrorMsg", "The given list is empty");
            ("catErrorMsg", "Expected a categorical variable but encountered a continuous one");
            ("catStrErrorMsg", "Expected a categorical variable of type string but encountered something else");
            ("contErrorMsg", "Expected a continuous variable but encountered a categorical one");
            ("catIntParseErrorMsg", "Failed to parse the given string as int");
            ("catBoolParseErrorMsg", "Failed to parse the given string as a bool");
            ("contFltParseErrorMsg", "Failed to parse the given string as float");
            (
            "unknownDataTypeParseErrorMsg",
            "Encountered an unknown DataType specification; failed to parse the given string")]
        |> Map.ofSeq


    let parseDataTableFromFile (filePath : string) : DataTable =
        let fileLines =
            filePath
            |> File.ReadAllLines
            |> Array.map (fun s ->
                s.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun t -> t.Trim())
                |> List.ofArray)
        let colHdrs =
            fileLines.[1]
            |> List.map (DataType.Cat << CatType.Str)
            |> Array.ofList
        let tblDat =
            [[|fileLines.[0]|]; fileLines.[2..]]
            |> Array.concat
            |> List.ofArray
            |> ListExtensions.transpose
            |> List.map (fun s ->
                match (List.head s) with
                | "catStr" -> s |> List.tail |> List.map (DataType.Cat << CatType.Str)
                | "catInt" -> s |> List.tail |> List.map (DataType.Cat << CatType.Int << int)
                | "catBool" -> s |> List.tail |> List.map (DataType.Cat << CatType.Bool << ((=) "true"))
                | "contFlt" -> s |> List.tail |> List.map (DataType.Cont << ContType.Flt << float)
                | _ -> failwith errorMsgs.["unknownDataTypeParseErrorMsg"])
            |> ListExtensions.transpose
            |> List.map Array.ofList
        colHdrs :: tblDat


    let coreImpurityFn<'A when 'A : equality> (classVals : list<'A>) : list<float> =
        let classValsLen = float (List.length classVals)
        classVals
        |> Seq.groupBy (id)
        |> Seq.map (fun (s, t) -> (float (Seq.length t)) / classValsLen)
        |> List.ofSeq


    let entropy (classVals : list<DataType>) : float =
        classVals
        |> coreImpurityFn
        |> List.map (fun s -> s * Math.Log(s, 2.0))
        |> (List.sum >> (( * ) (-1.0)))


    let giniIndex (classVals : list<DataType>) : float =
        classVals
        |> coreImpurityFn
        |> List.map (fun s -> s * s)
        |> (List.sum >> ((-) 1.0))


    let classificationError (classVals : list<DataType>) : float =
        classVals
        |> coreImpurityFn
        |> (List.max >> ((-) 1.0))


    let standardDeviationError (classVals : list<DataType>) : float =
        0.0


    let getInfoGainForCatVar
        (tblDat : DataTable)
        (idx : int)
        (impurityFn : ImpurityFn)
        (datSetImpurity : float)
        : InfoGainRes =
        let tblDatLen = float(List.length tblDat)
        tblDat
        |> Seq.groupBy (fun s -> Array.get s idx)
        |> Seq.map (fun (_, s) ->
            s
            |> Seq.map (fun t -> Array.get t ((Array.length t) - 1))
            |> (fun t -> float(Seq.length t), (impurityFn << List.ofSeq) t)
            |> (fun (t, u) -> t * u))
        |> Seq.sum
        |> (fun s -> {InfoGainRes.SplittingValOpt = None; InfoGainRes.InfoGain = datSetImpurity - (s / tblDatLen)})


    let defFltExtractorFn (sq : seq<DataType>) : seq<float> =
        sq
        |> Seq.map (fun s ->
            match s with
            | DataType.Cont(ContType.Flt t) -> t
            | _ -> failwith errorMsgs.["contErrorMsg"])


    let applyExOp (exFn : seq<'A> -> seq<'B>) (op : seq<'B> -> 'C) (sq : seq<'A>) : 'C =
        if Seq.isEmpty sq then failwith errorMsgs.["emptyLstErrorMsg"]
        else sq |> exFn |> op


    let getInfoGainForContVar
        (tblDat : DataTable)
        (idx : int)
        (impurityFn : ImpurityFn)
        (datSetImpurity : float)
        : InfoGainRes =
        let sortedTblDat = tblDat |> List.sortBy (fun s -> s.[idx])
        let sortedTblDatLen = float(List.length sortedTblDat)
        let rowLen = (Array.length << List.head) sortedTblDat
        sortedTblDat
        |> Seq.pairwise
        |> Seq.map (fun (s, t) ->
            DataType.op_Division(
                DataType.op_Addition(s.[idx], t.[idx]),
                DataType.Cont(ContType.Flt 2.0)))
        |> Seq.map (fun s ->
            sortedTblDat
            |> List.partition (fun t -> t.[idx] < s)
            |> (fun (t, u) -> [t; u])
            |> List.map (fun t -> float(List.length t) * impurityFn(t |> List.map (fun u -> u.[rowLen - 1])))
            |> List.sum
            |> (fun t -> applyExOp defFltExtractorFn Seq.head [s], datSetImpurity - (t / sortedTblDatLen)))
        |> (fun s ->
                Seq.fold
                    (fun t (u, v) ->
                        if t.InfoGain > v then t
                        else {InfoGainRes.SplittingValOpt = Some u; InfoGainRes.InfoGain = v})
                    (let (t, u) = Seq.head s in {InfoGainRes.SplittingValOpt = Some t; InfoGainRes.InfoGain = u})
                    (Seq.skip 1 s))


    let getInfoGain
        (tblDat : DataTable)
        (idx : int)
        (impurityFn : ImpurityFn)
        (datSetImpurity : float)
        : InfoGainRes =
        match Array.get (List.head tblDat) idx with
        | DataType.Cat _ -> getInfoGainForCatVar tblDat idx impurityFn datSetImpurity
        | _ -> getInfoGainForContVar tblDat idx impurityFn datSetImpurity


    let getTblDatSplitsForCatVar (tblDat : DataTable) (idx : int) : list<DataTable> =
        tblDat
        |> Seq.groupBy (fun s -> s.[idx])
        |> Seq.map (snd >> List.ofSeq)
        |> List.ofSeq


    let getTblDatSplitsForContVar (tblDat : DataTable) (idx : int) (infoGainRes : InfoGainRes) : list<DataTable> =
        let exFn (sq : seq<array<DataType>>) : seq<bool * array<DataType>> =
            sq
            |> Seq.map (fun s ->
                match s.[idx], infoGainRes.SplittingValOpt with
                | DataType.Cont(ContType.Flt t), Some u -> (t < u), s
                | _ -> failwith errorMsgs.["contErrorMsg"])
        let op (sq : seq<bool * array<DataType>>) : seq<DataTable> =
            sq
            |> List.ofSeq
            |> List.partition (fst)
            |> (fun (s, t) -> [List.map snd s; List.map snd t] |> Seq.ofList)
        (applyExOp exFn op (tblDat |> List.sortBy (fun s -> s.[idx]))) |> List.ofSeq


    let getTblDatSplits (tblDat : DataTable) (idx : int) (infoGainRes : InfoGainRes) : list<DataTable> =
        match (List.head tblDat).[idx] with
        | DataType.Cat _ -> getTblDatSplitsForCatVar tblDat idx
        | DataType.Cont _ -> getTblDatSplitsForContVar tblDat idx infoGainRes


    let getExcisedComponentsForCatVar (tblsLst : list<DataTable>) (idx : int) : list<ExcisedComponents> =
        let exFn (idx : int) (sqTbl: seq<seq<DataType>>) : seq<string * DataType * seq<seq<DataType>>> =
            if (Seq.isEmpty sqTbl) || (((Seq.skip idx) >> Seq.head >> Seq.length) sqTbl) < 2
            then failwith errorMsgs.["emptyLstErrorMsg"]
            else
                let colName, colVal =
                    let tmp = ((Seq.skip idx) >> Seq.head >> (Seq.take 2)) sqTbl in Seq.head tmp, Seq.last tmp
                match colName, colVal with
                | DataType.Cat(CatType.Str colNameStr), DataType.Cat _ -> seq [colNameStr, colVal, sqTbl]
                | _ -> failwith errorMsgs.["catErrorMsg"]
        let op (sq : seq<string * DataType * seq<seq<DataType>>>) : ExcisedComponents =
            let colName, colVal, sqTbl = Seq.head sq
            {ColName = colName;
            ColVal = colVal;
            ExcisedTable =
                (if idx < 1 then sqTbl |> Seq.skip 1
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


    let (defSplitStopCriterion : SplitStopCriterion) = fun (sqTbl : seq<seq<DataType>>) -> (Seq.length sqTbl) <= 4


    let getExcisedComponentsForContVar
        (tblsLst : list<DataTable>)
        (idx : int)
        (infoGainRes : InfoGainRes)
        (splitStopCriterion : SplitStopCriterion)
        : list<ExcisedComponents> =
        let exFn (idx : int) (transSqTbl : seq<seq<DataType>>) : seq<string * float * seq<seq<DataType>>> =
            if (Seq.isEmpty transSqTbl) || (((Seq.skip idx) >> Seq.head >> Seq.length) transSqTbl) < 2
            then failwith errorMsgs.["emptyLstErrorMsg"]
            else
                let colName, colVal =
                    let tmp = ((Seq.skip idx) >> Seq.head >> (Seq.take 2)) transSqTbl in Seq.head tmp, Seq.last tmp
                match colName, colVal with
                | DataType.Cat(CatType.Str colNameStr), DataType.Cont(ContType.Flt v) ->
                    seq [colNameStr, v, transSqTbl]
                | _ -> failwith errorMsgs.["contErrorMsg"]
        let op
            (idx : int)
            (splitStopCriterion : SplitStopCriterion)
            (sq : seq<string * float * seq<seq<DataType>>>)
            : ExcisedComponents =
            let colName, colVal, transSqTbl = Seq.head sq
            {
            ColName = colName;
            ColVal =
                let sv = infoGainRes.SplittingValOpt.Value
                (if colVal < sv then sv - epsilon else sv) |> (DataType.Cont << ContType.Flt);
            ExcisedTable =
                let splitStopFlg =
                    transSqTbl
                    |> ((Seq.map List.ofSeq) >> List.ofSeq)
                    |> ListExtensions.transpose
                    |> ((List.map Seq.ofList) >> Seq.ofList)
                    |> splitStopCriterion
                (if not splitStopFlg then transSqTbl
                elif idx < 1 then Seq.skip 1 transSqTbl
                else
                    seq {
                        yield! (Seq.take idx transSqTbl)
                        yield! (Seq.skip(idx + 1) transSqTbl)})
                |> Seq.map List.ofSeq
                |> List.ofSeq
                |> ListExtensions.transpose
                |> List.map (Array.ofList)}
        tblsLst
        |> List.map ((List.map List.ofArray) >> ListExtensions.transpose)
        |> List.map (Seq.map Seq.ofList)
        |> List.map (applyExOp (exFn idx) (op idx splitStopCriterion))


    let getExcisedComponents
        (tblsLst : list<DataTable>)
        (idx : int)
        (infoGainRes : InfoGainRes)
        (splitStopCriterionOpt : option<SplitStopCriterion>)
        : list<ExcisedComponents> =
        let exFn (idx : int) (tblsSq : seq<DataTable>) : seq<bool * seq<DataTable>> =
            let colType = ((Seq.head << (Seq.skip 1) << Seq.head) tblsSq).[idx]
            match colType with
            | DataType.Cat _ -> seq [true, tblsSq]
            | _ -> seq [false, tblsSq]
        let op (idx : int) (catFlgAndTblsSq : seq<bool * seq<DataTable>>) : list<ExcisedComponents> =
            let catFlg, tblsSq = Seq.head catFlgAndTblsSq
            if catFlg then getExcisedComponentsForCatVar (List.ofSeq tblsSq) idx
            else getExcisedComponentsForContVar (List.ofSeq tblsSq) idx infoGainRes splitStopCriterionOpt.Value
        applyExOp (exFn idx) (op idx) tblsLst


    let isSingleValuedDataTypeLst (lst : list<DataType>) : bool =
        Option.isNone (List.tryFind (not << ((=) (List.head lst))) (List.tail lst))


    let buildC45
        (tbl : DataTable)
        (impurityFn : ImpurityFn)
        (splitStopCriterionOpt : option<SplitStopCriterion>)
        : Node =
        let rec helper (currTbl : DataTable) : Node =
            let colHdrs = List.head currTbl
            let currTblDat = List.tail currTbl
            let currTblWidth = colHdrs |> Array.length
            let outputVals = currTblDat |> List.map (fun s -> s.[(Array.length s) - 1])
            let headOutputVal = List.head outputVals
            if isSingleValuedDataTypeLst outputVals then Node.Leaf headOutputVal
            else
                let datSetImpurity = impurityFn outputVals
                [0 .. currTblWidth - 2]
                |> List.map (fun s -> (s, getInfoGain currTblDat s impurityFn datSetImpurity))
                |> List.maxBy (fun (_, s) -> s.InfoGain)
                |> (fun (s, t) -> s, t, getTblDatSplits currTblDat s t)
                |> (fun (s, t, u) ->
                    getExcisedComponents (List.map (fun v -> colHdrs :: v) u) s t splitStopCriterionOpt)
                |> List.map (fun s ->
                    (DataType.Cat(CatType.Str s.ColName), s.ColVal),
                    (
                    if s.ExcisedTable |> (List.head >> Array.length >> ((=) 1)) then
                        s.ExcisedTable
                        |> List.tail
                        |> List.map (fun s -> Array.get s 0)
                        |> (fun s ->
                            if isSingleValuedDataTypeLst s then Node.Leaf(List.head s)
                            else Node.LeafList s)
                    else helper s.ExcisedTable))
                |> (Map.ofSeq >> Node.Internal)
        helper tbl


    let getPrediction (c45Tree : Node) (inputMap : Map<DataType, DataType>) : list<int * DataType> =
        let rec helper (currC45Tree : Node) : list<DataType> =
            match currC45Tree with
            | Node.Leaf v -> [v]
            | Node.LeafList lst -> lst
            | Node.Internal internalMap ->
                let internalMapSq = internalMap |> Map.toSeq
                let internalMapKeys = internalMapSq |> Seq.map fst
                let colHdr = internalMapKeys |> Seq.head |> fst
                if (Map.containsKey colHdr inputMap) then
                    let inputVal = inputMap.[colHdr]
                    match inputVal with
                    | DataType.Cat _ -> helper internalMap.[colHdr, inputVal]
                    | DataType.Cont _ ->
                        let maxKey = Seq.maxBy snd internalMapKeys
                        let minKey = Seq.minBy snd internalMapKeys
                        if inputVal < snd maxKey then helper internalMap.[minKey]
                        else helper internalMap.[maxKey]
                else
                    internalMapSq
                    |> Seq.map snd
                    |> List.ofSeq
                    |> List.collect helper
        c45Tree
        |> helper
        |> Seq.groupBy id
        |> Seq.map (fun (s, t) -> Seq.length t, s)
        |> List.ofSeq

