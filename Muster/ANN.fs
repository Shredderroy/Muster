namespace Muster.DataStructuresAndAlgorithms


open System
open System.IO
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MusterLib


module ANN =


    [<RequireQualifiedAccess>]
    type Scaler =
        {Forward : double -> double;
        Inverse : double -> double}


    [<RequireQualifiedAccess>]
    type Network =
        {Layers : list<Matrix<double>>;
        InputActivation : double -> double;
        DInputActivation : double -> double;
        OutputActivation : double -> double;
        DOutputActivation : double -> double;
        LearningParam : double;
        InputScaling : list<Scaler>;
        OutputScaling : list<Scaler>}


    [<RequireQualifiedAccess>]
    type TrainingMode = Single | Batch


    [<RequireQualifiedAccess>]
    type TrainingSet = {InputVecsMat : Matrix<double>; OutputVecsMat : Matrix<double>}


    type ForwardPassOutput = list<Matrix<double> * Matrix<double>>


    type BackPropagatedError = list<Matrix<double>>


    type ScalingOutput =
        {ScaledTrainingSet : TrainingSet;
        InputScaling : list<Scaler>;
        OutputScaling: list<Scaler>}


    let rnd = Random()


    let useNativeMKLProvider () =
        Control.NativeProviderPath <- (
            Path.Combine [|
                __SOURCE_DIRECTORY__; ".."; "packages"; "MathNet.Numerics.MKL.Win-x64.2.2.0"; "build"; "x64"
            |])
        Control.UseNativeMKL()


    let useManagedProvider () = Control.UseManaged()


    useNativeMKLProvider()


    let init
        (layerConfig : list<int>)
        (inputActivation : double -> double)
        (dInputActivation : double -> double)
        (outputActivation : double -> double)
        (dOutputActivation : double -> double)
        (learningParam : double)
        : Network =
        {
        Network.Layers =
            layerConfig
            |> Seq.pairwise
            |> Seq.map (fun (s, t) ->
                DenseMatrix.init s t (fun _ _ -> let x = rnd.NextDouble() in if rnd.Next() % 2 = 0 then x else -x))
            |> List.ofSeq;
        Network.InputActivation = inputActivation;
        Network.DInputActivation = dInputActivation;
        Network.OutputActivation = outputActivation;
        Network.DOutputActivation = dOutputActivation;
        Network.LearningParam = learningParam;
        Network.InputScaling = [];
        Network.OutputScaling = []}


    let importTrainingSetFromFile (filePath : string) (inputDim : int) (outputDim : int) : TrainingSet =
        File.ReadAllLines filePath
        |> Array.map (fun s ->
            let t = (s.Split [|','|]) |> Array.map (fun u -> double (u.Trim())) in
            t.[..(inputDim - 1)], t.[inputDim..])
        |> Array.fold (fun (s, t) (u, v) -> ((List.ofArray u) :: s), ((List.ofArray v) :: t)) ([], [])
        |> (fun (s, t) ->
            {TrainingSet.InputVecsMat = matrix (List.rev s); TrainingSet.OutputVecsMat = matrix (List.rev t)})


    let defaultInputActivation (x : double) = 1.0 / (1.0 + exp(-x))


    let defaultDInputActivation (x : double) = let y = defaultInputActivation x in y * (1.0 - y)


    let defaultOutputActivation (x : double) = x


    let defaultDOutputActivation (_ : double) = 1.0


    let vectorToList (vec : Vector<double>) : list<double> = vec |> Vector.toList


    let genScaler (leftEndPt : double) (rightEndPt : double) : Scaler =
        {
        Scaler.Forward =
            if leftEndPt < rightEndPt then (fun s -> 2.0 * (s - leftEndPt) / (rightEndPt - leftEndPt) - 1.0)
            else (fun _ -> 0.0);
        Scaler.Inverse =
            if leftEndPt < rightEndPt then (fun s -> ((rightEndPt - leftEndPt) * (s + 1.0) / 2.0) + leftEndPt)
            else (fun _ -> leftEndPt)}


    let scaleTrainingSet (trainingSet : TrainingSet) : ScalingOutput =
        let getScaling (m : Matrix<double>) =
            m |> Matrix.toColArrays |> Array.map (fun s -> genScaler (Array.min s) (Array.max s))
        let scaleVec (scaling : array<Scaler>) = Array.map2 (fun (s : Scaler) t -> s.Forward t) scaling
        let convertArrToMat = Array.map (List.ofArray) >> List.ofArray >> matrix
        let scaleMatrix scaling = Matrix.toRowArrays >> (Array.map (scaleVec scaling)) >> convertArrToMat
        let inputScaling, outputScaling = getScaling trainingSet.InputVecsMat, getScaling trainingSet.OutputVecsMat
        let scaledInputVecsMat = trainingSet.InputVecsMat |> (scaleMatrix inputScaling)
        let scaledOutputVecsMat = trainingSet.OutputVecsMat |> (scaleMatrix outputScaling)
        {
        ScaledTrainingSet =
            {TrainingSet.InputVecsMat = scaledInputVecsMat; TrainingSet.OutputVecsMat = scaledOutputVecsMat};
        InputScaling = (inputScaling |> List.ofSeq);
        OutputScaling = (outputScaling |> List.ofSeq)}


    let makeForwardPass (aNN : Network) (scaledInputVecsMat : Matrix<double>) : ForwardPassOutput =
        (aNN.Layers, ([for _ in 1 .. ((List.length aNN.Layers) - 1) -> aNN.InputActivation] @ [aNN.OutputActivation]))
        ||> List.zip
        |> List.scan
            (fun (_, (s : Matrix<double>)) (t, u) -> let prod = s * t in (prod, (Matrix.map u prod)))
            (scaledInputVecsMat, scaledInputVecsMat)


    let backPropagateError
        (aNN : Network)
        (scaledOutputVecsMat : Matrix<double>)
        (forwardPassOutput : ForwardPassOutput)
        : BackPropagatedError =
        let lastOutputV, lastOutputY = forwardPassOutput |> Seq.last
        let errorMat = scaledOutputVecsMat - lastOutputY
        let derMat = errorMat.PointwiseMultiply(lastOutputV |> Matrix.map aNN.DOutputActivation)
        ((aNN.Layers |> List.rev), (forwardPassOutput |> List.rev |> List.tail))
        ||> List.zip
        |> List.scan
            (fun (s : Matrix<double>) (t, (u, _)) ->
                (t * s).PointwiseMultiply(Matrix.transpose(Matrix.map aNN.DInputActivation u)))
            (derMat |> Matrix.transpose)
        |> List.rev


    let correctWeights
        (aNN : Network)
        (forwardPassOutput : ForwardPassOutput)
        (backPropagatedError : BackPropagatedError)
        : Network =
        {aNN with Network.Layers =
                    [((forwardPassOutput |> List.rev |> List.tail |> List.rev), (List.tail backPropagatedError))
                    ||> List.map2 (fun (_, s) t -> aNN.LearningParam * Matrix.transpose(t * s));
                    aNN.Layers]
                    |> ListExtensions.mapThread (List.reduce (+))}


    let singleTrain (aNN : Network) (scaledTrainingSet : TrainingSet) (numOfEpochs : int) : Network =
        let zippedLst =
            Array.zip
                ((Matrix.toRowArrays scaledTrainingSet.InputVecsMat) |> Array.map (fun s -> matrix [List.ofArray s]))
                ((Matrix.toRowArrays scaledTrainingSet.OutputVecsMat) |> Array.map (fun s -> matrix [List.ofArray s]))
        (aNN, [1 .. numOfEpochs])
        ||> List.fold
            (fun s _ ->
                (s, zippedLst)
                ||> Array.fold
                    (fun t ((u : Matrix<double>), (v : Matrix<double>)) ->
                        let forwardPassOutput = makeForwardPass t u
                        let backPropagatedError = backPropagateError t v forwardPassOutput
                        correctWeights t forwardPassOutput backPropagatedError))


    let batchTrain (aNN : Network) (scaledTrainingSet : TrainingSet) (numOfEpochs : int) : Network =
        (aNN, [1 .. numOfEpochs])
        ||> Seq.fold
            (fun s _ ->
                let forwardPassOutput = makeForwardPass s scaledTrainingSet.InputVecsMat
                let backPropagatedError = backPropagateError s scaledTrainingSet.OutputVecsMat forwardPassOutput
                correctWeights s forwardPassOutput backPropagatedError)


    let train (aNN : Network) (trainingSet : TrainingSet) (numOfEpochs : int) (trainingMode : TrainingMode) : Network =
        let scaledTrainingSet, inputScaling, outputScaling =
            let scalingOutput = scaleTrainingSet trainingSet
            scalingOutput.ScaledTrainingSet, scalingOutput.InputScaling, scalingOutput.OutputScaling
        let tANN =
            match trainingMode with
            | TrainingMode.Single -> singleTrain aNN scaledTrainingSet numOfEpochs
            | TrainingMode.Batch -> batchTrain aNN scaledTrainingSet numOfEpochs
        {tANN with Network.InputScaling = inputScaling; Network.OutputScaling = outputScaling}


    let getPrediction (aNN : Network) (inputVec : Vector<double>) : Vector<double> =
        (aNN.InputScaling, vectorToList inputVec)
        ||> List.map2 (fun s t -> s.Forward t)
        |> (fun s -> makeForwardPass aNN (matrix [s]))
        |> (snd << Seq.last)
        |> (vectorToList << Seq.head << (fun s -> s.EnumerateRows()))
        |> List.map2 (fun (s : Scaler) t -> s.Inverse t) aNN.OutputScaling
        |> vector


    let getRMSDForTrainingSet (aNN : Network) (trainingSet : TrainingSet) : double =
        trainingSet.InputVecsMat
        |> (DenseMatrix.ofRowSeq << (Seq.map (getPrediction aNN)) << Matrix.toRowSeq)
        |> (-) trainingSet.OutputVecsMat
        |> Matrix.map (fun s -> s * s)
        |> Matrix.mapRows (fun _ s -> vector [Vector.sum s])
        |> (fun s -> (Matrix.reduce (+) s) / (double (Matrix.rowCount s)))
        |> sqrt

    