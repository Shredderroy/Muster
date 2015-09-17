namespace Muster.DataStructuresAndAlgorithms


module RandomForest =


    type Forest = list<DecisionTree.Node>


    [<RequireQualifiedAccess>]
    type SampleSize =
        | Int of int
        | Pct of float


    val buildWithParams :
        DecisionTree.DataTable ->
        int ->
        SampleSize ->
        DecisionTree.ImpurityFn ->
        option<DecisionTree.SplitStopCriterion> ->
        Forest


    val buildDefault : DecisionTree.DataTable -> int -> SampleSize -> Forest

