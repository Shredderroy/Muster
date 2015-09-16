namespace Muster.DataStructuresAndAlgorithms


open System


module RandomForest =


    type Forest = list<DecisionTree.Node>


    type SubsetNum =
        | Int of int
        | Pct of float


    let buildWithParams (tbl : DecisionTree.DataTable) (b : int) (subsetNum : SubsetNum) : Forest =
        //
        []


    let build (tbl : DecisionTree.DataTable) : Forest =
        //
        []


    let test () : unit = ()

