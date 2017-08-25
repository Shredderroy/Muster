namespace MusterLib


open Microsoft.FSharp.Control.WebExtensions
open System
open System.Net


module Scraper =


    type URL = string


    type HTML = string


    [<RequireQualifiedAccess>]
    type Index = | First | Last | N of int


    let strSplit (chrs : array<char>) (flg : bool) (str : string) : list<string> =
        if flg then str.Split(chrs, StringSplitOptions.RemoveEmptyEntries) else str.Split chrs
        |> Array.map (fun s -> s.Trim()) |> List.ofArray


    let removeTags (tagNames : list<string>) (html : HTML) : string =
        let rec helper (currHtml : HTML) (currTagName : string) : string =
            let openTag = "<" + currTagName
            let closeTag = "</" + currTagName + ">"
            let closeTagIdx = currHtml.IndexOf closeTag
            let openTagIdx =
                if closeTagIdx = -1 then currHtml.IndexOf openTag
                else currHtml.LastIndexOf(openTag, closeTagIdx)
            if openTagIdx = -1 then currHtml
            elif closeTagIdx = -1 then
                let closeIdx = currHtml.IndexOf('>', openTagIdx)
                helper (currHtml.Remove(openTagIdx, closeIdx - openTagIdx + 1)) currTagName
            else helper (currHtml.Remove(openTagIdx, closeTagIdx + closeTag.Length - openTagIdx)) currTagName
        List.fold helper html tagNames


    let removeWords (words : list<string>) (html : HTML) : string =
        (html, words) ||> List.fold (fun s t -> s.Replace(t, ""))


    let extractInnermostTags (tags : list<string>) (html : HTML) : Map<string, list<HTML>> =
        let rec helper (startIdx : int) (acc : list<string>) (currTagName : string) : string * list<string> =
            let openTag = "<" + currTagName
            let closeTag = "</" + currTagName + ">"
            let closeTagIdx = html.IndexOf(closeTag, startIdx)
            let openTagIdx =
                if closeTagIdx = -1 then html.IndexOf(openTag, startIdx)
                else html.LastIndexOf(openTag, closeTagIdx)
            if (openTagIdx < startIdx) || (openTagIdx = -1) then currTagName, List.rev acc
            elif closeTagIdx = -1 then
                let closeIdx = html.IndexOf('>', openTagIdx)
                helper (closeIdx + 1) ((html.Substring(openTagIdx, closeIdx - openTagIdx + 1)) :: acc) currTagName
            else
                helper
                    (closeTagIdx + 1)
                    ((html.Substring(openTagIdx, closeTagIdx + closeTag.Length - openTagIdx)) :: acc)
                    currTagName
        tags |> List.map (helper 0 []) |> Map.ofList


    let extractInnermostTag (tag : string) (html : HTML) : list<string> =
        html |> extractInnermostTags [tag] |> Map.toList |> List.head |> snd


    let extractLinkFromATag (aTag : string) : string =
        let getStartIdx (words : list<string>) (str : string) : int =
            List.fold (fun (s : int) (t : string) -> max s (str.IndexOf(t, s))) 0 words
        let startIdx = getStartIdx ["href"; "="; "\""] aTag
        let endIdx = let idx = aTag.IndexOf("\"", startIdx + 1) in if idx = -1 then aTag.Length else idx
        aTag.Substring(startIdx + 1, endIdx - startIdx - 1)


    let extractTextFromTag (tag : string) : string =
        let startIdx = tag.IndexOf('>')
        if startIdx = -1 then ""
        else
            let endIdx = tag.IndexOf('<', startIdx)
            if endIdx = -1 then "" else tag.Substring(startIdx + 1, endIdx - startIdx - 1)


    let getTagAtIndex (tag : string) (idx : Index) (extractedTags : Map<string, list<HTML>>) : HTML =
        if extractedTags |> Map.containsKey tag then
            match idx, extractedTags.[tag], List.length extractedTags.[tag] with
            | Index.First, h :: _, _ -> h
            | Index.Last, s, l when l > 0 -> s |> List.last
            | Index.N n, s, l when n < l -> s |> (List.head << (List.skip n))
            | _ -> ""
        else ""


    let fetchHtmlAsync (url : URL) : HTML =
        async {
            try
                let uri = new System.Uri(url)
                let webClient = new WebClient()
                let! html = webClient.AsyncDownloadString uri
                return html
            finally
                ()
        } |> Async.RunSynchronously

