// SETUP 
let evaluate x = x
                    |>Array.map (fun xx -> (xx-0.5)*(xx-0.5))
                    |>Array.sum
         

let nvar = 30
let npop = 90
let nparent = 20
let ngen = 1000
let elitismApply=1
let nelite = 1
let pmut = 0.01
let rngpar= MathNet.Numerics.Distributions.DiscreteUniform(0,nparent-1)
let rng= MathNet.Numerics.Distributions.ContinuousUniform(0.0,1.0)
//End SETUP

//Initialization
let pop = EA.initializer nvar npop
let popy = pop |> EA.parallelEvaluator evaluate  
let (elitey,elite)= EA.elitism nelite pop popy [||] [||]
                                                |>Array.unzip

let Initial = (pop,popy,elite,elitey)

printfn "%A  %A" (Array.min popy) (Array.min elitey)
//End Initialization

let iterator = [|1..ngen|]
let a= iterator      
        |>Array.fold (fun state i -> 
                    let (pop,popy,elite,elitey)=state
                    let parent = EA.parentselect pop popy nparent "Turnament"
                    let popNew = [|1..npop|]
                                    |>Array.map (fun i -> EA.xover parent.[rngpar.Sample()] parent.[rngpar.Sample()] "SBX")
                                    |>Array.map (fun indiv -> 
                                     match (rng.Sample() < pmut) with
                                        | true -> EA.mutate indiv "Uniform"
                                        | false -> indiv
                                        )
                                    |> EA.elitismApply elitismApply popy elite elitey
                                    |> Array.map EA.correction
                                
                    let popyNew = popNew |> EA.evaluator evaluate
                    let (eliteyNew,eliteNew)= EA.elitism nelite popNew popyNew elite elitey
                                                |>Array.unzip
                    printfn "%A %A %A" i (Array.min eliteyNew) (eliteNew)
                    (popNew,popyNew,eliteNew,eliteyNew)      
                    ) Initial 


  