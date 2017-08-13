namespace MusterLib


open System
open System.Text.RegularExpressions


module StringExtensions =


    let genWord (rnd : Random) (wordLen : int) : string =
        fun _ -> (97 + (rnd.Next() % 26)) |> (char >> string)
        |> ((List.init wordLen) >> (String.concat ""))


    let genSentence (rnd : Random) (numWords : int) (maxWordLen : int) : string =
        fun _ -> genWord rnd (1 + (rnd.Next() % maxWordLen))
        |> ((List.init numWords) >> (String.concat " "))
        |> fun s -> (s |> (Seq.head >> int >> ((+) (-32)) >> char >> string)) + (s.Substring 1) + "."


    let getTokens (str : string) =
        [for s in str.Split([|' '|]) do let t = s.Trim() in if not(String.IsNullOrEmpty t) then yield t]


    let getNGrams (str : string) : list<string> =
        let f = (fun s t -> [t] @ [for u in s -> t + " " + u] @ s)
        List.fold f [] (List.rev (getTokens str))


    let getShingles (str : string) : list<string> =
        let f = (fun s (i, t) -> [t] @ [for u in (Seq.take i s) -> t + " " + u] @ s)
        List.fold f [] (let toks = getTokens str in List.zip [0 .. (List.length toks) - 1] (List.rev toks))


    let getMaximalItems (lst : list<string>) : list<string> =
        let f = (fun s t -> (Seq.tryFind(fun (u : string) -> u.Contains t) s) |> Option.isSome)
        let g = (fun s t u -> not ((f s u) || (f t u)))
        let zLst = Seq.mapi (fun i s -> (i, s)) lst
        [for (s, t) in zLst do if g (Seq.take s lst) (Seq.skip (s + 1) lst) t then yield t]


    let replaceGroups (openTag : string) (closeTag : string) (repStr : string) (str : string) : string =
        let rec helper (sIdx : int) (currStr : string) : string =
            if sIdx >= currStr.Length then currStr
            else
                let idxC = (currStr.Substring(sIdx).IndexOf closeTag) + sIdx
                if idxC <= sIdx then currStr
                else
                    let idxO = currStr.Substring(0, idxC).LastIndexOf(openTag)
                    if (idxO = -1) then currStr
                    elif idxO >= idxC then helper (idxO + 1) currStr
                    else helper (idxO + 1) (currStr.Replace(currStr.Substring(idxO, idxC - idxO + 1), repStr))
        helper 0 str


    let getAllLongestSubstrings (numChrs : int) (ltOrEqFlg : bool) (str : string) : list<string> =
        let rec helper
            (resAcc : list<int * int * int>)
            (currStartIdx : int) (currIdx : int)
            (currAcc : Set<char>)
            (chrsArr : array<char>)
            : list<int * int * int> =
            if currIdx = Array.length chrsArr then resAcc
            else
                let bestLen = match resAcc with [] -> 0 | (_, h2, _) :: _ -> h2
                let newAcc = Set.add (Array.get chrsArr currIdx) currAcc
                let newAccLen = Set.count newAcc
                let currLen = currIdx - currStartIdx + 1
                let notOverflow, newMatchAsGoodOrBetter = (newAccLen <= numChrs), (currLen >= bestLen)
                if notOverflow && newMatchAsGoodOrBetter then
                    helper
                        (match resAcc with
                        | (_, h2, _) :: _ when h2 < currLen -> [currStartIdx, currLen, newAccLen]
                        | _ -> ((currStartIdx, currLen, newAccLen) :: resAcc))
                        currStartIdx
                        (currIdx + 1)
                        newAcc
                        chrsArr
                elif not notOverflow then helper resAcc (currStartIdx + 1) (currStartIdx + 1) Set.empty chrsArr
                else helper resAcc currStartIdx (currIdx + 1) newAcc chrsArr
        str.ToCharArray() |> helper [] 0 0 Set.empty
        |> List.filter (fun (_, _, s) -> ltOrEqFlg || (s = numChrs))
        |> List.map (fun (s, t, _) -> str.Substring(s, t))
        |> List.rev


    let compress (str : string) : string = (new Regex(@"\s+")).Replace(str, " ")


    let removeNonAlphaNumChars (str : string) : string = compress((new Regex(@"[^0-9A-Za-z]")).Replace(str, " "))


    let removeApostrophe (str : string) : string = compress(str.Replace('\'', ' '))


    let isInteger (str : string) : bool = (new Regex(@"^[0-9]+$")).IsMatch str


    let isFloat (str : string) : bool = (new Regex(@"(^[0-9]?\.[0-9]+$)|(^[0-9]+)")).IsMatch str

