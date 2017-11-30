namespace MusterLib


open System


module MapExtensions =


    let mergeWith (m1 : Map<'A, 'B>) (f : option<'C> -> option<'B> -> 'D) (m0 : Map<'A, 'C>) : Map<'A, 'D> =
        let k0, k1 = m0 |> (Map.toSeq >> Seq.map fst >> Set.ofSeq), m1 |> (Map.toSeq >> Seq.map fst >> Set.ofSeq)
        (k0, k1) ||> Set.union |> Set.toSeq
        |> Seq.map (fun k ->
            k,
            f
                (if m0 |> Map.containsKey k then Some(m0.[k]) else None)
                (if m1 |> Map.containsKey k then Some(m1.[k]) else None)
        ) |> Map.ofSeq

