namespace Muster.DataStructuresAndAlgorithms


open System


module RandomForest =


    type Forest = list<DecisionTree.Node>


    [<RequireQualifiedAccess>]
    type SampleSize =
        | Int of int
        | Pct of float


    let errorMsgs =
        seq [
            ("sampleSizeErrorMsg", "Invalid specification of SampleSize")]
        |> Map.ofSeq


    let buildWithParams (tbl : DecisionTree.DataTable) (b : int) (sampleSize: SampleSize) : Forest =
        let sampleSize =
            match sampleSize with
            | SampleSize.Int v -> v
            | SampleSize.Pct p when (p > 0.0) && (p < 1.0) ->
                tbl
                |> List.length
                |> float
                |> ( * ) p
                |> Math.Floor
                |> int
            | _ -> failwith errorMsgs.["sampleSizeErrorMsg"]
        //
        []


    let build (tbl : DecisionTree.DataTable) : Forest =
        //
        []


    let test () : unit = ()

