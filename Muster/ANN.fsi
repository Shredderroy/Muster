namespace Muster.DataStructuresAndAlgorithms


open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open Muster.Extensions


module ANN =


    [<Sealed>]
    type Network


    [<RequireQualifiedAccess>]
    type TrainingMode = Single | Batch


    [<RequireQualifiedAccess>]
    type TrainingSet = {InputVecsMat : Matrix<double>; OutputVecsMat : Matrix<double>}


    val init :
        list<int> ->
        (double -> double) ->
        (double -> double) ->
        (double -> double) ->
        (double -> double) ->
        double ->
        Network


    val importTrainingSetFromFile : string -> int -> int -> TrainingSet


    val defaultInputActivation : double -> double


    val defaultDInputActivation : double -> double


    val defaultOutputActivation : double -> double


    val defaultDOutputActivation : double -> double


    val train : Network -> TrainingSet -> int -> TrainingMode -> Network


    val getPrediction : Network -> Vector<double> -> Vector<double>


    val getRMSDForTrainingSet : Network -> TrainingSet -> double

