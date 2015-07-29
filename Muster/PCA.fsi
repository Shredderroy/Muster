namespace Muster.DataStructuresAndAlgorithms


open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double


module PCA =


    val changeBasis : Matrix<double> -> Matrix<double>


    val reduceMatDim : double -> Matrix<double> -> Matrix<double> -> Matrix<double>


    val reduceVecDim : int -> Matrix<double> -> Vector<double> -> Vector<double>


