module JiebaSegmenter

open System
open System.Text
open System.IO
open System.Text.RegularExpressions
open Predefine
open Util
open model

let RegexChineseDefault = Regex @"([\u4E00-\u9FD5a-zA-Z0-9+#&\._]+)"
let RegexSkipDefault = Regex @"(\r\n|\s)"
let RegexChineseCutAll = Regex @"([\u4E00-\u9FD5]+)"
let RegexSkipCutAll = Regex @"[^a-zA-Z0-9+#\n]"
let RegexEnglishChars = Regex @"[a-zA-Z0-9]"
let RegexUserDict = Regex "^(?<word>.+?)(?<freq> [0-9]+)?(?<tag> [a-z]+)?$"
let Locker = obj
let mutable LoadedPaths = Set.empty<string>
let mutable UserWordTagTab = Map.empty<string, string>
let WordDict = WordDictionary.Instance

let cutIt (blk: string) (cutMethod: string -> string list) (reHan:Regex) (reSkip: Regex) cutAll =
    if System.String.IsNullOrWhiteSpace(blk) then []
    else
        match reHan.IsMatch blk with
        | true -> cutMethod blk
        | _ ->
            reSkip.Split blk |> 
                Array.map (fun x -> if reSkip.IsMatch x || cutAll then [x] else x |> Seq.map (fun ch -> ch.ToString()) |> List.ofSeq) |> List.concat


let rec getSubStr4Dag k i N (sentence: string) (tempList: int list) =
    match i < N with
    | false -> if tempList.Length > 0 then tempList else [k]
    | _ -> 
        let frag = sentence.Substring(k, i+1 - k)
        let newTempList = if WordDict.Trie.ContainsKey frag && WordDict.Trie.[frag] > 0.0 then List.append tempList [i] else tempList
        getSubStr4Dag k (i+1) N sentence newTempList 
        
let getDag (sentence: string) =
    let N = sentence.Length
    sentence.ToCharArray() |> Array.mapi (fun k c -> (k, getSubStr4Dag k k N sentence [])) |> List.ofArray


let cutAll (sentence: string) = 
    let rec getWords lastPos words (dag: (int * int list) list) =
        match dag with
        | [] -> words
        | h :: t ->
            let k, nexts = h
            if nexts.Length = 1 && k > lastPos
            then getWords nexts.Head (sentence.Substring(k, nexts.Head + 1 - k):: words) t
            else
                let newWords, newLastPos = nexts |> List.fold (fun (ws, lp) n -> if n <= k then (ws, lp) else (sentence.Substring(k, n + 1 - k):: ws, n)) (words, lastPos)
                getWords newLastPos newWords t
    getDag sentence |> getWords -1 [] |> List.rev
   
let calc (sentence: string) (dag : (int * int list) list) =
    let logtotal = Math.Log WordDict.Total
    Map.empty<int, int * double>.Add(sentence.Length, (0, 0.0)) |> List.foldBack
        (fun (i, nexts) (route: Map<int, int *double>) -> 
                nexts |> List.map (fun x -> 
                    x, sentence.Substring(i, x + 1 - i) |> WordDict.GetFreq |> Math.Log |> (+) ((route.[x+1] |> snd) - logtotal)
                ) |> List.maxBy (fun (k, v) -> v) |> composeTuple i |> route.Add
        )
        dag

// In reversed order to add each segment into words
let addBuffer2WordList (words: string list) (buf: string) =
    match buf.Length with
    | 1 ->  buf :: words
    |_ -> 
        if WordDict.containsWord buf
        then buf |> Seq.fold (fun ws s -> s.ToString() :: ws) words
        else
            FinalSeg.cut buf |> List.fold (fun ws s -> s :: ws) words

let cutDag (sentence : string) =
    let n = sentence.Length
    let dag = getDag sentence
    let route = calc sentence dag
    let rec addWords x words (buf: string) =
        match x < n with
        | false -> 
            match buf.Length > 0 with
            | false -> words
            | _ -> addBuffer2WordList words buf
        | _ ->
            let y = fst route.[x] + 1
            let w = sentence.Substring(x, y - x)
            match y - x with
            | 1 -> addWords y words (buf + w)
            | _ ->
                let tempWords = if buf.Length > 0 then addBuffer2WordList words buf else words
                addWords y (w :: tempWords) String.Empty

    addWords 0 [] String.Empty |> List.rev  // at the last step, string list should be reversed

let cutDagWithoutHmm (sentence : string) =
    let n = sentence.Length
    let dag = getDag sentence
    let route = calc sentence dag
    let rec addWords x words (buf: string) =
        match x < n with
        | false ->
            match buf.Length > 0 with
            | false -> words
            | _ -> buf :: words
        | _ ->
            let y = fst route.[x] + 1
            let l_word = sentence.Substring(x, y - x)
            match RegexEnglishChars.IsMatch l_word && l_word.Length = 1 with
            | true -> addWords y words (buf + l_word)
            | _ ->
                let tempWords = if buf.Length > 0 then buf :: words else words
                addWords y (l_word :: tempWords) String.Empty

    addWords 0 [] String.Empty |> List.rev      // at last step, reverse the string list

let cut text cutAll_b hmm = 
    let reHan = if cutAll_b then RegexChineseCutAll else RegexChineseDefault
    let reSkip = if cutAll_b then RegexSkipCutAll else RegexSkipDefault
    let cutMethod = if cutAll_b then cutAll elif hmm then cutDag else cutDagWithoutHmm
    reHan.Split text |> Array.fold (fun res blk -> res @ cutIt blk cutMethod reHan reSkip cutAll_b) []

let cut_dotnet (text, cutAll_b, hmm) = cut text cutAll_b hmm |> Array.ofList

let cut4Search (text: string) hmm =
    let fineSegment (word: string) words =
        match word.Length > 2 with
        | false -> word::words
        | _ ->
            let newWords = {0 .. word.Length - 2} |> Seq.map (fun i -> word.Substring(i, 2)) |> Seq.filter (fun w -> WordDict.containsWord w)
                            |> Seq.fold (fun ws w -> w :: ws) words
            if word.Length = 3 then word :: newWords
            else
                {0 .. word.Length - 3} |> Seq.map (fun i -> word.Substring(i, 3)) |> Seq.filter (fun w -> WordDict.containsWord w)
                    |> Seq.fold (fun ws w -> w :: ws) newWords |> composeTuple word |> List.Cons

    cut text false hmm |> List.fold (fun ws w -> fineSegment w ws) [] |> List.rev

let cut4Search_dotnet (text: string, hmm: bool) = cut4Search text hmm |> Array.ofList

let addWord (word: string) (freq: float) tag =
    if String.IsNullOrEmpty(tag) then () else UserWordTagTab <- UserWordTagTab.Add(word, tag)

    (if freq <= 0.0 then cut word false false |> WordDict.SuggestFreq word else freq) |> WordDict.addWord word

let addWord_dotnet (word:string, freq: double, tag) = addWord word freq tag

let deleteWord word = WordDict.deleteWord word

let LoadUserDict path =
    if LoadedPaths.Contains path then ()
    else
        let getWords line words =
            match String.IsNullOrWhiteSpace(line) with
            | true -> words
            | _ -> 
                let tokens = RegexUserDict.Match(line.Trim()).Groups
                let freq = tokens.["freq"].Value.Trim()
                let freq_i = if freq.Length > 0 then float freq else 0.0
                (tokens.["word"].Value.Trim(), freq_i, tokens.["tag"].Value.Trim()) :: words

        let loadDict () =
            if LoadedPaths.Contains path then ()
            else
                File.ReadLines(path, Encoding.UTF8) |> Seq.iter (fun line -> getWords line [] |> List.iter (fun (w, f, t) -> addWord w f t))
        lock Locker loadDict

    

