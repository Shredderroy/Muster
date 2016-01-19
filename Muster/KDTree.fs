namespace Muster.DataStructuresAndAlgorithms


open System
open System.IO


module KDTree =


    type Vec<'A when 'A : comparison> = array<option<'A>>


    [<RequireQualifiedAccess; CustomEquality; CustomComparison>]
    type Node<'A when 'A : comparison> =
        {LeftChild : option<Node<'A>>;
        RightChild : option<Node<'A>>;
        SplittingAxis : int;
        ID : int;
        Vec : array<'A>}
        override this.Equals yObj =
            match yObj with 
            | :? Node<'A> as y -> this.ID = y.ID
            | _ -> false
        override this.GetHashCode() = this.ID
        interface IComparable with
            member this.CompareTo yObj =
                match yObj with
                | :? Node<'A> as y -> compare this.ID y.ID
                | _ -> invalidArg "yObj" "Cannot compare objects of different types"


    [<RequireQualifiedAccess>]
    type Query<'A when 'A : comparison> =
        | Just of Vec<'A>
        | JustInRadius of Vec<'A> * int
        | ClosedBounds of Vec<'A> * Vec<'A>
        | ClosedOpenBounds of Vec<'A> * Vec<'A>
        | OpenClosedBounds of Vec<'A> * Vec<'A>
        | OpenBounds of Vec<'A> * Vec<'A>
        | And of list<Query<'A>>
        | Or of list<Query<'A>>
        | Diff of list<Query<'A>>


    type DiffFunc<'A when 'A : comparison> = option<'A> -> 'A -> int


    type DistFunc<'A when 'A : comparison> = DiffFunc<'A> -> Vec<'A> -> array<'A> -> int


    [<RequireQualifiedAccess>]
    type Path = Left | Right


    [<RequireQualifiedAccess>]
    type Zipper<'A when 'A : comparison> =
        | Start
        | Internal of Node<'A> * Path * Zipper<'A>
        | End of Node<'A> * Zipper<'A>


    let applyZipperOp
        (kDT : Node<'A>)
        (endFunc : Node<'A> -> 'B -> bool)
        (pathFunc : Node<'A> -> 'B -> Path)
        (modFunc : Node<'A> -> 'B -> Node<'A>)
        (funcParam : 'B)
        : Node<'A> =
        let rec unzip (currNode : Node<'A>) (currZipper : Zipper<'A>) : Zipper<'A> =
            match
                (endFunc currNode funcParam),
                (pathFunc currNode funcParam),
                currNode.LeftChild,
                currNode.RightChild with
            | false, Path.Left, Some leftChild, _ ->
                unzip leftChild (Zipper.Internal(currNode, Path.Left, currZipper))
            | false, Path.Right, _, Some rightChild ->
                unzip rightChild (Zipper.Internal(currNode, Path.Right, currZipper))
            | _ -> Zipper.End((modFunc currNode funcParam), currZipper) 
        let rec zip (currZipper : Zipper<'A>) : option<Node<'A>> =
            match currZipper with
            | Zipper.End(currNode, Zipper.Internal(parNode, Path.Left, remZipper)) ->
                zip(Zipper.End({parNode with Node.LeftChild = Some currNode}, remZipper))
            | Zipper.End(currNode, Zipper.Internal(parNode, Path.Right, remZipper)) ->
                zip(Zipper.End({parNode with Node.RightChild = Some currNode}, remZipper))
            | Zipper.End(currNode, Zipper.Start) -> Some currNode
            | _ -> None
        match zip(unzip kDT Zipper.Start) with None -> kDT | Some newNode -> newNode


    let rnd = Random()


    let nullFunc (b : int) (v : 'A) : option<'A> =
        let hp = 10 in if rnd.Next() % hp < b then Some v else None


    let genRandIntVec (dim : int) (maxVal : int) : array<int> =
        Array.init dim (fun _ -> rnd.Next() % maxVal)


    let genRandTestIntVec (dim : int) (maxVal : int) (b : int) : Vec<int> =
        (genRandIntVec dim maxVal) |> Array.map (nullFunc b)


    let genRandIntVecsArr (dim : int) (numOfVecs : int) (maxVal : int) : array<array<int>> =
        Array.init numOfVecs (fun _ -> genRandIntVec dim maxVal)


    let genRandInt16Vec (dim : int) (maxVal : int16) : array<int16> =
        Array.init dim (fun _ -> int16 (rnd.Next() % (int maxVal)))


    let genRandTestInt16Vec (dim : int) (maxVal : int16) (b : int) : Vec<int16> =
        (genRandInt16Vec dim maxVal) |> Array.map (nullFunc b)


    let genRandInt16VecsArr (dim : int) (numOfVecs : int) (maxVal : int16) : array<array<int16>> =
        Array.init numOfVecs (fun _ -> genRandInt16Vec dim maxVal)


    let genRandCharVec (dim : int) : array<char> =
        Array.init dim (fun _ -> (97 + rnd.Next() % 26) |> char)


    let genRandTestCharVec (dim : int) (b : int) : Vec<char> =
        (genRandCharVec dim) |> Array.map (nullFunc b)


    let genRandCharVecsArr (dim : int) (numOfVecs : int) : array<array<char>> =
        Array.init numOfVecs (fun _ -> genRandCharVec dim)


    let genRandDblVec (dim : int) (maxVal : double) : array<double> =
        Array.init dim (fun _ -> rnd.NextDouble() * maxVal)


    let genRandTestDblVec (dim : int) (maxVal : double) (b : int) : Vec<double> =
        (genRandDblVec dim maxVal) |> Array.map (nullFunc b)


    let genRandDblVecsArr (dim : int) (numOfVecs : int) (maxVal : double) : array<array<double>> =
        Array.init numOfVecs (fun _ -> genRandDblVec dim maxVal)


    let buildWithStartParams
        (vecsArr : array<array<'A>>)
        (startDepthOpt : option<int>)
        (rootIDOpt : option<int>)
        : option<Node<'A>> =
        let rec helper
            (currDepth : int)
            (currNodeID : int)
            (currVecsArr : array<array<'A>>)
            : option<Node<'A>> =
            if currVecsArr.Length > 1 then
                let currSplittingAxis = currDepth % currVecsArr.[0].Length
                let sortedVecsArr =
                    currVecsArr
                    |> Array.sortBy (fun s -> s.[currSplittingAxis])
                let medIndex = sortedVecsArr.Length / 2
                let medValue = sortedVecsArr.[medIndex].[currSplittingAxis]
                let splittingIndex =
                    sortedVecsArr
                    |> Array.findIndex (fun s -> s.[currSplittingAxis] = medValue)
                {
                Node.LeftChild =
                    helper
                        (currDepth + 1)
                        (currNodeID + 1) 
                        (if splittingIndex = 0 then [||] else sortedVecsArr.[..(splittingIndex - 1)]);
                Node.RightChild =
                    helper
                        (currDepth + 1)
                        (currNodeID + splittingIndex + 1)
                        (sortedVecsArr.[(splittingIndex + 1)..]);
                Node.SplittingAxis = currSplittingAxis;
                Node.ID = currNodeID;
                Node.Vec = sortedVecsArr.[splittingIndex]} |> Some
            elif currVecsArr.Length = 1 then
                {Node.LeftChild = None;
                Node.RightChild = None;
                Node.SplittingAxis = currDepth % currVecsArr.[0].Length;
                Node.ID = currNodeID;
                Node.Vec = currVecsArr.[0]} |> Some
            else
                None
        helper
            (if startDepthOpt.IsNone then 0 else startDepthOpt.Value)
            (if rootIDOpt.IsNone then 0 else rootIDOpt.Value)
            vecsArr


    let build (vecsArr : array<array<'A>>) : option<Node<'A>> = buildWithStartParams vecsArr None None


    let getMaxNodeID (kDT : Node<'A>) : int =
        let rec helper (currNode : Node<'A>) : int =
            max
                currNode.ID
                (max
                    (match currNode.LeftChild with | Some leftChild -> helper leftChild | _ -> -1)
                    (match currNode.RightChild with | Some rightChild -> helper rightChild | _ -> -1))
        helper kDT


    let addVec (kDT : Node<'A>) (vec : array<'A>) : Node<'A> =
        let newNodeID = (getMaxNodeID kDT) + 1
        let endFunc (_ : Node<'A>) ((_, _) : array<'A> * int) = false
        let pathFunc (node : Node<'A>) ((insVec, _) : array<'A> * int) : Path =
            if insVec.[node.SplittingAxis] < node.Vec.[node.SplittingAxis] then Path.Left else Path.Right
        let modFunc (node : Node<'A>) ((insVec, insNodeID) : array<'A> * int) : Node<'A> =
            let newNode =
                    {Node.LeftChild = None;
                    Node.RightChild = None;
                    Node.ID = insNodeID;
                    Node.SplittingAxis = (node.SplittingAxis + 1) % node.Vec.Length;
                    Node.Vec = insVec}
            if insVec.[node.SplittingAxis] < node.Vec.[node.SplittingAxis]
            then {node with Node.LeftChild = Some newNode}
            else {node with Node.RightChild = Some newNode}
        applyZipperOp kDT endFunc pathFunc modFunc (vec, newNodeID)


    let buildFromChildNodes (parNode : Node<'A>) (newRootNodeID : int) : option<Node<'A>> =
        let rec helper (currNodesLst : list<Node<'A>>) (acc : list<array<'A>>) : array<array<'A>> =
            match currNodesLst with
            | [] -> acc |> Array.ofList
            | _ ->
                helper
                    (currNodesLst
                    |> List.collect (fun s -> [s.LeftChild; s.RightChild])
                    |> List.choose id)
                    ((currNodesLst |> List.map (fun s -> s.Vec)) @ acc)
        buildWithStartParams
            (helper ([parNode.LeftChild; parNode.RightChild] |> List.choose id) [])
            (Some parNode.SplittingAxis)
            (Some newRootNodeID)


    let removeJustNode (kDT : Node<'A>) (remNode : Node<'A>) : option<Node<'A>> =
        let newRootNodeID = (getMaxNodeID kDT) + 1
        let endFunc (node : Node<'A>) ((_, remNodeID, _) : array<'A> * int * int) : bool =
            (node.LeftChild.IsSome && (node.LeftChild.Value.ID = remNodeID)) ||
            (node.RightChild.IsSome && (node.RightChild.Value.ID = remNodeID))
        let pathFunc (node : Node<'A>) ((remVec, _, _) : array<'A> * int * int) : Path =
            if remVec.[node.SplittingAxis] < node.Vec.[node.SplittingAxis] then Path.Left else Path.Right
        let modFunc (node : Node<'A>) ((_, _, newRootNodeID) : array<'A> * int * int) : Node<'A> =
            let leftFlg = node.LeftChild.IsSome && (node.LeftChild.Value.ID = remNode.ID)
            let tgChild = if leftFlg then node.LeftChild.Value else node.RightChild.Value
            let modNode = buildFromChildNodes tgChild newRootNodeID
            if leftFlg then {node with Node.LeftChild = modNode} else {node with Node.RightChild = modNode}
        if kDT.ID = remNode.ID
        then buildFromChildNodes kDT newRootNodeID
        else Some(applyZipperOp kDT endFunc pathFunc modFunc (remNode.Vec, remNode.ID, newRootNodeID))


    let removeNodeAndDescendants (kDT : Node<'A>) (remNode : Node<'A>) : option<Node<'A>> =
        let endFunc (node : Node<'A>) ((_, remNodeID) : array<'A> * int) : bool =
            (node.LeftChild.IsSome && (node.LeftChild.Value.ID = remNodeID)) ||
            (node.RightChild.IsSome && (node.RightChild.Value.ID = remNodeID))
        let pathFunc (node : Node<'A>) ((remVec, _) : array<'A> * int) : Path =
            if remVec.[node.SplittingAxis] < node.Vec.[node.SplittingAxis] then Path.Left else Path.Right
        let modFunc (node : Node<'A>) (_ : array<'A> * int) : Node<'A> =
            if node.LeftChild.IsSome && (node.LeftChild.Value.ID = remNode.ID)
            then {node with Node.LeftChild = None}
            else {node with Node.RightChild = None}
        if kDT.ID = remNode.ID
        then None
        else Some(applyZipperOp kDT endFunc pathFunc modFunc (remNode.Vec, remNode.ID))


    let findNodesWithVec (kDT : Node<'A>) (vec : array<'A>) : list<Node<'A>> =
        let rec helper (currNode : Node<'A>) (acc : list<Node<'A>>) : list<Node<'A>> =
            if vec.[currNode.SplittingAxis] >= currNode.Vec.[currNode.SplittingAxis] then
                let newAcc = if currNode.Vec = vec then currNode :: acc else acc
                if currNode.RightChild.IsSome then helper currNode.RightChild.Value newAcc else newAcc
            elif currNode.LeftChild.IsSome then helper currNode.LeftChild.Value acc
            else acc
        helper kDT []


    let removeVec (kDT : Node<'A>) (vec : array<'A>) : option<Node<'A>> =
        List.fold
            (fun s t -> match s with Some node -> removeJustNode node t | _ -> s)
            (Some kDT)
            (findNodesWithVec kDT vec)


    let rebuild (kDT : Node<'A>) : option<Node<'A>> =
        buildFromChildNodes
            {Node.LeftChild = Some kDT;
            Node.RightChild = None;
            Node.SplittingAxis = 0;
            Node.ID = 0;
            Node.Vec = kDT.Vec}
            0


    let (intDiffFunc : DiffFunc<int>) = (fun s t -> match s with Some u -> u - t | _ -> 0)


    let (int16DiffFunc : DiffFunc<int16>) = (fun s t -> match s with Some u -> int(u - t) | _ -> 0)


    let (charDiffFunc : DiffFunc<char>) = (fun s t -> match s with Some u -> int u - int t | _ -> 0)


    let dblDiffFunc (numOfSigDigits : int) : DiffFunc<double> =
        let m = (double 10)**(double numOfSigDigits) in
        (fun s t -> match s with Some u -> int (m * (u - t)) | _ -> 0)


    let euclDistFunc (diffFunc : DiffFunc<'A>) (vec1 : Vec<'A>) (vec2 : array<'A>) : int =
        (vec1, vec2)
        ||> Array.map2 (fun s t -> let u = diffFunc s t in u * u)
        |> Array.sum


    let getBestMatches
        (kDT : Node<'A>)
        (distFunc : DistFunc<'A>)
        (diffFunc : DiffFunc<'A>)
        (testVec : Vec<'A>)
        (radOpt : option<int>)
        : list<Node<'A>> =
        let rec descendTree (currNode : Node<'A>) (parAcc : list<Node<'A>>) : list<Node<'A>> =
            let currNodeVecVal = currNode.Vec.[currNode.SplittingAxis]
            match
                testVec.[currNode.SplittingAxis],
                currNode.LeftChild,
                currNode.RightChild with
            | Some s, Some leftChild, _ when s < currNodeVecVal -> descendTree leftChild (currNode :: parAcc)
            | Some s, None, Some rightChild when s < currNodeVecVal -> descendTree rightChild (currNode :: parAcc)
            | Some s, _, Some rightChild when s >= currNodeVecVal -> descendTree rightChild (currNode :: parAcc)
            | Some s, Some leftChild, None when s >= currNodeVecVal -> descendTree leftChild (currNode :: parAcc)
            | None, _, Some child | None, Some child, None -> descendTree child (currNode :: parAcc)
            | _ ->
                checkBestMatchesAcc
                    ([currNode]
                    |> List.filter (fun s -> (radOpt.IsNone) || ((distFunc diffFunc testVec s.Vec) <= radOpt.Value)))
                    parAcc
        and checkBestMatchesAcc (currBestMatchesAcc : list<Node<'A>>) (parAcc : list<Node<'A>>) : list<Node<'A>> =
            match parAcc with
            | [] -> currBestMatchesAcc
            | currParNode :: restParAcc ->
                let currParDist = distFunc diffFunc testVec currParNode.Vec
                let planeSplittingDist =
                    diffFunc
                        testVec.[currParNode.SplittingAxis]
                        currParNode.Vec.[currParNode.SplittingAxis]
                let planeSplittingDistSq = planeSplittingDist * planeSplittingDist
                let newBestMatchesAcc, newTargetDist =
                    match radOpt with
                    | Some rad ->
                        if currParDist <= rad
                        then (currParNode :: currBestMatchesAcc), rad
                        else currBestMatchesAcc, rad
                    | None ->
                        let currBestMatchesDist = distFunc diffFunc testVec (currBestMatchesAcc |> List.head).Vec
                        if (currBestMatchesDist < currParDist) then (currBestMatchesAcc, currBestMatchesDist)
                        elif (currBestMatchesDist = currParDist) then (currParNode :: currBestMatchesAcc, currBestMatchesDist)
                        else ([currParNode], currParDist)
                match
                    planeSplittingDistSq <= newTargetDist,
                    currParNode.LeftChild,
                    currParNode.RightChild with
                | true, Some leftChild, Some rightChild ->
                    let sibBestMatchesAcc = descendTree (if planeSplittingDist < 0 then rightChild else leftChild) []
                    let sibBestMatchesDist = distFunc diffFunc testVec (sibBestMatchesAcc |> List.head).Vec
                    if sibBestMatchesDist < newTargetDist
                    then checkBestMatchesAcc sibBestMatchesAcc restParAcc
                    elif sibBestMatchesDist = newTargetDist
                    then checkBestMatchesAcc (newBestMatchesAcc @ sibBestMatchesAcc) restParAcc
                    else checkBestMatchesAcc newBestMatchesAcc restParAcc
                | _ -> checkBestMatchesAcc newBestMatchesAcc restParAcc
        descendTree kDT []


    let inline checkNodeVecForClosedBounds (nodeVec : array<'A>) (vecLB : Vec<'A>) (vecUB : Vec<'A>) : bool =
        (Array.zip3 nodeVec vecLB vecUB)
        |> Array.fold (fun s (t, u, v) ->
            s && (
                match u, v with
                | Some w, Some x -> (w <= t) && (x >= t)
                | Some w, None -> w <= t
                | None, Some w -> w >= t
                | None, None -> true))
            true


    let inline checkNodeVecForClosedOpenBounds (nodeVec : array<'A>) (vecLB : Vec<'A>) (vecUB : Vec<'A>) : bool =
        (Array.zip3 nodeVec vecLB vecUB)
        |> Array.fold (fun s (t, u, v) ->
            s && (
                match u, v with
                | Some w, Some x -> (w <= t) && (x > t)
                | Some w, None -> w <= t
                | None, Some w -> w > t
                | None, None -> true))
            true


    let inline checkNodeVecForOpenClosedBounds (nodeVec : array<'A>) (vecLB : Vec<'A>) (vecUB : Vec<'A>) : bool =
        (Array.zip3 nodeVec vecLB vecUB)
        |> Array.fold (fun s (t, u, v) ->
            s && (
                match u, v with
                | Some w, Some x -> (w < t) && (x >= t)
                | Some w, None -> w < t
                | None, Some w -> w >= t
                | None, None -> true))
            true


    let inline checkNodeVecForOpenBounds (nodeVec : array<'A>) (vecLB : Vec<'A>) (vecUB : Vec<'A>) : bool =
        (Array.zip3 nodeVec vecLB vecUB)
        |> Array.fold (fun s (t, u, v) ->
            s && (
                match u, v with
                | Some w, Some x -> (w < t) && (x > t)
                | Some w, None -> w < t
                | None, Some w -> w > t
                | None, None -> true))
            true


    let getRangeMatches
        (kDT : Node<'A>)
        (vecLB : Vec<'A>)
        (vecUB : Vec<'A>)
        (checkFunc : (array<'A> -> Vec<'A> -> Vec<'A> -> bool))
        : list<Node<'A>> =
        let rec helper (currNodesLst : list<Node<'A>>) (acc : list<Node<'A>>) : list<Node<'A>> =
            match currNodesLst with
            | [] -> acc
            | _ ->
                let newNodesLst =
                    currNodesLst
                    |> List.collect (fun s ->
                        let coord = s.SplittingAxis
                        let nodeVecVal = s.Vec.[coord]
                        match s.LeftChild, s.RightChild, vecLB.[coord], vecUB.[coord] with
                        | Some leftChild, _, _, Some t when t < nodeVecVal -> [leftChild]
                        | _, Some rightChild, Some t, _ when t >= nodeVecVal -> [rightChild]
                        | _ -> List.choose id [s.LeftChild; s.RightChild])
                helper
                    newNodesLst
                    ((List.filter (fun s -> checkFunc s.Vec vecLB vecUB) newNodesLst) @ acc)
        helper
            [kDT]
            (if checkFunc kDT.Vec vecLB vecUB then [kDT] else [])


    let runNNQuery
        (kDT : Node<'A>)
        (distFunc : DistFunc<'A>)
        (diffFunc : DiffFunc<'A>)
        (query : Query<'A>)
        : Set<Node<'A>> =
        let rec helper (currQuery : Query<'A>) : Set<Node<'A>> =
            match currQuery with
            | Query.Just v -> (getBestMatches kDT distFunc diffFunc v None) |> Set.ofList
            | Query.JustInRadius(v, r) -> getBestMatches kDT distFunc diffFunc v (Some r) |> Set.ofList
            | Query.And lst -> lst |> List.map helper |> Set.intersectMany
            | Query.Or lst -> lst |> List.map helper |> Set.unionMany
            | Query.Diff lst -> lst |> List.map helper |> List.reduce (Set.difference)
            | _ -> Set.empty
        helper query


    let runNNQueryAsync
        (kDT : Node<'A>)
        (distFunc : DistFunc<'A>)
        (diffFunc : DiffFunc<'A>)
        (query : Query<'A>)
        : Async<Set<Node<'A>>> =
        async {return runNNQuery kDT distFunc diffFunc query}


    let runRangeQuery (kDT : Node<'A>) (query : Query<'A>) : Set<Node<'A>> =
        let rec helper (currQuery : Query<'A>) : Set<Node<'A>> =
            match currQuery with
            | Query.ClosedBounds(vecLB, vecUB) ->
                (getRangeMatches kDT vecLB vecUB checkNodeVecForClosedBounds) |> Set.ofList
            | Query.ClosedOpenBounds(vecLB, vecUB) ->
                (getRangeMatches kDT vecLB vecUB checkNodeVecForClosedOpenBounds) |> Set.ofList
            | Query.OpenClosedBounds(vecLB, vecUB) ->
                (getRangeMatches kDT vecLB vecUB checkNodeVecForOpenClosedBounds) |> Set.ofList
            | Query.OpenBounds(vecLB, vecUB) ->
                (getRangeMatches kDT vecLB vecUB checkNodeVecForOpenBounds) |> Set.ofList
            | Query.And lst -> lst |> List.map helper |> Set.intersectMany
            | Query.Or lst -> lst |> List.map helper |> Set.unionMany
            | Query.Diff lst -> lst |> List.map helper |> List.reduce (Set.difference)
            | _ -> Set.empty
        helper query


    let runRangeQueryAsync (kDT : Node<'A>) (query : Query<'A>) : Async<Set<Node<'A>>> =
        async {return runRangeQuery kDT query}
            

    let getBestMatchesByLinearScanning
        (vecsArr : array<array<'A>>)
        (testVec : Vec<'A>)
        (distFunc : DistFunc<'A>)
        (diffFunc : DiffFunc<'A>)
        : list<array<'A>> =
        vecsArr.[1..]
        |> Array.fold (fun (s, t) u ->
                let d = (distFunc diffFunc testVec u)
                if d < s then (d, [u]) elif d = s then (d, u :: t) else (s, t))
                (distFunc diffFunc testVec vecsArr.[0], [vecsArr.[0]])
        |> snd


    let getClosedRangeMatchesByLinearScanning
        (vecsArr : array<array<'A>>)
        (vecLB : Vec<'A>)
        (vecUB : Vec<'A>)
        : array<array<'A>> =
        vecsArr
        |> Array.filter (fun s -> checkNodeVecForClosedBounds s vecLB vecUB)


    let getClosedOpenRangeMatchesByLinearScanning
        (vecsArr : array<array<'A>>)
        (vecLB : Vec<'A>)
        (vecUB : Vec<'A>)
        : array<array<'A>> =
        vecsArr
        |> Array.filter (fun s -> checkNodeVecForClosedOpenBounds s vecLB vecUB)


    let getOpenClosedRangeMatchesByLinearScanning
        (vecsArr : array<array<'A>>)
        (vecLB : Vec<'A>)
        (vecUB : Vec<'A>)
        : array<array<'A>> =
        vecsArr
        |> Array.filter (fun s -> checkNodeVecForOpenClosedBounds s vecLB vecUB)


    let getOpenRangeMatchesByLinearScanning
        (vecsArr : array<array<'A>>)
        (vecLB : Vec<'A>)
        (vecUB : Vec<'A>)
        : array<array<'A>> =
        vecsArr
        |> Array.filter (fun s -> checkNodeVecForOpenBounds s vecLB vecUB)

