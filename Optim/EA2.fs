module EA2 
open System
let rng= MathNet.Numerics.Distributions.ContinuousUniform(0.0,1.0)



///Initializes population
let addddd (nvar:int) (npop:int) =
    Array.init npop (
            fun index -> Array.init nvar (
                            fun ele -> rng.Sample()
                                         )
                    )

