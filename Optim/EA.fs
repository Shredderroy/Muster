module EA 
open System
let rng= MathNet.Numerics.Distributions.ContinuousUniform(0.0,1.0)



///Initializes population
let initializer (nvar:int) (npop:int) =
    Array.init npop (
            fun index -> Array.init nvar (
                            fun ele -> rng.Sample()
                                         )
                    )

/// Applys crossover (of type: xoverType) to parents p1 and p2 
let xover (p1:float[]) (p2:float[]) xoverType =
    match xoverType with
    | "Simple" -> Array.map2 (fun pp1 pp2 -> (pp1+pp2) / 2.0) p1 p2
    | "Intermediate" -> Array.map2 (fun pp1 pp2 -> pp1 + rng.Sample()*(pp2-pp1)) p1 p2
    | "SBX" -> Array.map2 
                    (fun pp1 pp2 ->
                        let SBXnu = 2.0 //Nu =2
                        match (rng.Sample() < 0.5) with
                        | true -> 
                            let SBXbeta = (2.0*rng.Sample())**SBXnu
                            pp1-0.5*SBXbeta*(pp2-pp1)
                        | false -> 
                            let SBXbeta = (1.0/(2.0*rng.Sample()))**(SBXnu+2.0)
                            pp1+0.5*SBXbeta*(pp2-pp1)
                        ) p1 p2
    | _ -> raise(Exception("xoverType not awailable"))

/// Applys mutation (of type: mutateType) to offspring o1 
let mutate (o1:float[]) mutateType =
    let rad=0.001
    match mutateType with
    | "Simple" -> Array.map 
                    ( fun oo1 -> 
                       match (rng.Sample() < 0.5) with
                       | true -> oo1 + rad * rng.Sample()
                       | false -> oo1 - rad * rng.Sample()
                       ) o1
    | "Uniform" -> Array.map 
                    ( fun oo1 -> 
                       match (rng.Sample() < 0.5) with
                       | true -> oo1 + (1.0 - oo1) * rng.Sample()
                       | false -> oo1 - oo1 * rng.Sample()
                       ) o1
    | _ -> raise(Exception("mutateType not awailable"))

///Respect bounds [0,1]
let correction (o1:float[]) =
    o1
     |>Array.map (fun oo1 ->
                    match (oo1 < 0.0) with 
                     | true -> 0.0
                     | false ->  match (oo1 > 1.0) with
                                    |true -> 1.0
                                    |false -> oo1
     )

/// Applys parent sellection to population pop 
let parentselect (pop:float[][]) (popy:float[]) (parents:int) parentselectType =
    match parentselectType with
    | "Elitist" -> 
        (popy, pop)
        ||> Array.zip
        |> Array.sortBy (fun elem -> fst(elem))
        |> Seq.take parents
        |> Seq.toArray 
        |> Array.map snd
    | "Turnament" -> let rngdi=MathNet.Numerics.Distributions.DiscreteUniform(0 , Array.length(pop)-1)
                     let turnamendsize = 10   
                     [|1..parents|]
                         |> Array.map ( fun i ->
                                        let  xs = [| for x in 1..turnamendsize  ->  rngdi.Sample()|]
                                        let popyxs = xs
                                                      |>Array.map (fun i -> popy.[i]) 

                                        let popxs = xs
                                                    |>Array.map (fun i -> pop.[i]) 

                                        (popyxs, popxs)
                                                    ||> Array.zip
                                                    |> Array.minBy (fun elem -> fst(elem))
                                                    |> snd
                                                )   
    | _ -> raise(Exception("parentselectType not awailable"))

///Updates Elite set
let elitism (elites:int) (pop:float[][]) (popy:float[]) (elite:float[][]) (elitey:float[]) =
     Array.concat [| Array.zip elitey elite;  Array.zip popy pop|]
       |> Array.sortBy (fun elem -> fst(elem))
       |> Seq.take elites
       |> Seq.toArray 

///Updates population (replacing worst individuals with elites)
let elitismApply (elitesApply:int) (popy:float[]) (elite:float[][]) (elitey:float[]) (pop:float[][]) =
     let popnew=  (popy, pop)
                    ||> Array.zip
                    |> Array.sortBy (fun elem -> fst(elem))
                    |> Seq.take ((Array.length pop) - elitesApply) 
                    |> Seq.toArray
                    |> Array.map snd
     Array.concat [| popnew; elite.[(Array.length(elite)-elitesApply)..(-1+Array.length(elite))] |]

    
/// Evaluates population
let evaluator (evaluate:float[]->float) (pop:float[][]) = pop
                                                        |>Array.map (fun p -> evaluate p)


/// Evaluates population using parallelMap
let parallelEvaluator (evaluate:float[]->float) (pop:float[][]) = pop
                                                                    |>Array.Parallel.map (fun p -> evaluate p)
