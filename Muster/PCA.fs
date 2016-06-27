namespace Muster.DataStructuresAndAlgorithms


open System
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MusterLib


module PCA =


    let useNativeMKLProvider () =
        Control.NativeProviderPath <- @"C:\Users\aroy\OneDrive\Repositories\Muster\packages\MathNet.Numerics.MKL.Win-x64.2.0.0\build\x64"
        Control.UseNativeMKL()


    useNativeMKLProvider()


    let changeBasis (mat : Matrix<double>) : Matrix<double> =
        let matT = Matrix.transpose mat
        (Matrix.eigen (matT * mat)).EigenVectors
        |> Matrix.toColSeq
        |> (List.ofSeq << (Seq.map List.ofSeq))
        |> ListExtensions.mapThread (List.rev)
        |> matrix


    let reduceMatDim (eps : double) (basisMat : Matrix<double>) (mat : Matrix<double>) : Matrix<double> =
        (mat * basisMat)
        |> Matrix.toColSeq
        |> Seq.takeWhile (fun s -> (Vector.exists (fun t -> abs t > eps) s))
        |> (List.ofSeq << (Seq.map List.ofSeq))
        |> (Matrix.transpose << matrix)


    let reduceVecDim (numOfCoordsToSkip : int) (basisMat : Matrix<double>) (vec : Vector<double>) : Vector<double> =
        (vec * basisMat)
        |> Seq.skip numOfCoordsToSkip
        |> (vector << List.ofSeq)

