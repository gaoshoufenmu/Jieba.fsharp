module PosSeg
open System
open System.Text.RegularExpressions
open System.IO
open System.Text
open Predefine
open Util
open model

open JiebaSegmenter

let RegexChineseInternal = Regex @"([\u4e00-\u9fd5a-zA-Z0-9+#&\._]+)"
let RegexSkipInternal = Regex @"(\r\n|\s)"

let RegexChineseDetail = Regex @"([\u4e00-\u9fd5]+)"
let RegexSkipDetail = Regex @"([\.0-9]+|[a-zA-Z0-9]+)"
let RegexEnglishWords = Regex @"[a-zA-Z0-9]+"
let RegexNumbers = Regex @"[\.0-9]+"
let RegexEnglishChar = Regex @"^[a-zA-Z0-9]$"

let TransProbs = File.ReadAllText(PosProbTransFile) |> jsonDeserialize<Map<string, Map<string, double>>> 
let EmitProbs = File.ReadAllText(PosProbEmitFile) |> jsonDeserialize<Map<string, Map<char, double>>> 
let StartProbs = File.ReadAllText(PosProbStartFile) |> jsonDeserialize<Map<string, double>> 
let StateTab = File.ReadAllText(CharStateTabFile) |> jsonDeserialize<Map<char, string list>> 



let AllStates = getMapKeys TransProbs

let mutable WordTagTab = 
    let getWordTagTab (map:Map<string, string>) (line: string) =
        match line.Split(' ') with
        | [|word; _; tag|] -> map.Add(word, tag)
        |_ -> map

    File.ReadAllLines(MainDictFile, Encoding.UTF8) |> Array.fold (fun map line -> getWordTagTab map line) Map.empty<string, string>

let getWithDefaultStates ch =
    match StateTab.TryFind ch with
    | Some states -> states
    | _ -> AllStates

let getStartProbAndPath ch = 
    getWithDefaultStates ch |> List.fold 
        (fun (vm : Map<string, double>, pm : Map<string, string>) state -> 
            vm.Add(state, StartProbs.[state] + getDefaultProb state ch EmitProbs), pm.Add(state, ""))
        (Map.empty<string, double>, Map.empty<string, string>)

let getNextProbAndState (obsState: string) (prevStates: string list) (prevV: Map<string, double>) ch (vm: Map<string, double>, pm: Map<string, string>) =
    let emitProb = getDefaultProb obsState ch EmitProbs
    let prob, state = prevStates |> List.fold (fun tl ps -> (prevV.[ps] + getDefaultProb ps obsState TransProbs + emitProb, ps)  :: tl) []
                        |> List.maxBy (fun (k, v) -> k)
    vm.Add(obsState, prob), pm.Add(obsState, state)
    
// both vl and pl are in reversed order
let rec getProbAndPath chars (vl: Map<string, double> list) (pl: Map<string, string> list) = 
    match chars with
    | [] -> vl, pl
    | ch :: tail ->
        let prevStates = getMapKeys pl.Head |> List.filter (fun k -> TransProbs.[k].Count > 0)
        let curPossibleStates = prevStates |> List.fold (fun states s -> states @ getMapKeys TransProbs.[s]) []
        let obsStates = 
            let obsStatesInner = getWithDefaultStates ch |> Set.ofList |> Set.intersect (curPossibleStates |> Set.ofSeq)
            match obsStatesInner.Count > 0 with
            | true -> obsStatesInner |> Set.toList
            | _ -> if curPossibleStates.Length > 0 then curPossibleStates else AllStates

        let nv, np = obsStates |> List.fold (fun (vm, pm) y -> getNextProbAndState y prevStates vl.Head ch (vm, pm)) 
                                                (Map.empty<string, double>, Map.empty<string, string>)
        getProbAndPath tail (nv :: vl) (np:: pl)


let viterbiCut_Pos (sentence: string) =
    let vs, ps = getStartProbAndPath sentence.[0]
    let sentence_list = sentence |> List.ofSeq
    let vl, pl = getProbAndPath sentence_list.Tail [vs] [ps]
    let prob, state = getMapKeys pl.Head |> List.map (fun k -> vl.Head.[k], k) |> List.maxBy (fun (p, s) -> p)

    let route = pl |> List.fold (fun states pm -> pm.[states.Head] :: states) [state] |> List.tail  // Head is an empty string    // route is in positive order 
    
    let rec getSegments (pos_list: string list) (segs: (string*string) list) i b n (n_pos: string)=
        match pos_list with
        | [] -> 
            match n < i with
            | true -> (sentence.Substring(n), n_pos.Split('-').[1]) :: segs
            | _ -> segs

        | h :: t -> 
            let next = i + 1
            let fstChar = h.[0]
            let pos = h.Split('-').[1]
            let new_npos = if t.Length = 0 then String.Empty else t.Head
            match fstChar with
            | 'B' -> getSegments t segs next i n n_pos
            | 'E' -> getSegments t ((sentence.Substring(b, next - b), pos):: segs) next b next new_npos
            | 'S' -> getSegments t ((sentence.Substring(i, 1), pos)::segs) next b next new_npos
            | _ -> getSegments t segs next b n n_pos
    getSegments route [] 0 0 0 (route.Head)     // segment is in reverse order

let MergeMapB2A (A: Map<string, string>) (B: Map<string, string>) = 
    B |> Map.fold (fun (map: Map<string, string>) k v -> if map.ContainsKey k then map.Remove k |> Map.add k v else map.Add (k, v)) A

let checkNewUserWordTags () =
    if UserWordTagTab.IsEmpty then ()
    else
        WordTagTab <- MergeMapB2A UserWordTagTab WordTagTab
        UserWordTagTab <- Map.empty<string, string>

let cutDetail_Pos (text: string) =
    let getTokens (blk: string) tokens =
        match RegexChineseDetail.IsMatch blk with 
        | true -> viterbiCut_Pos blk @ tokens                       // reversed order
        | _ ->
            RegexSkipDetail.Split blk |> Array.fold (fun ts x -> 
                match String.IsNullOrWhiteSpace x with
                | true -> ts
                | _ ->
                    if RegexNumbers.IsMatch x then (x, "m") :: ts
                    elif RegexEnglishWords.IsMatch x then (x, "eng") :: ts
                    else (x, "x") :: ts
            ) tokens
    RegexChineseDetail.Split text |> Array.fold (fun ts blk -> getTokens blk ts) []     // reversed order

// words: reversed order
let addBuffer2WordList (buf: string) words =
    match buf.Length with 
    | 1 -> (buf, getMapValOrDefault buf WordTagTab "x") :: words
    | _ ->
        match WordDict.containsWord buf with
        | true -> buf.ToCharArray() |> Array.fold (fun ws ch -> (ch.ToString(), "x") :: ws) words
        | _ -> cutDetail_Pos buf @ words    

let cutDag_Pos (sentence: string) =
    let dag = getDag sentence
    let route = calc sentence dag
    let n = sentence.Length
    let rec getTokens x (buf: string) tokens =
        match x < n with 
        | false -> if buf.Length > 0 then addBuffer2WordList buf tokens else tokens 
        | _ ->
            let y = route.[x] |> fst |> (+) 1
            let w = sentence.Substring(x, y - x)
            match y - x with 
            | 1 -> getTokens y (buf + w) tokens
            | _ ->
                let wTokens = w, getMapValOrDefault w WordTagTab "x"
                let tempTokens = if buf.Length > 0 then addBuffer2WordList buf tokens |> composeTuple wTokens |> List.Cons else wTokens :: tokens
                getTokens y String.Empty tempTokens
    getTokens 0 String.Empty [] //|> List.rev     // reverse a reversed order tokens => get a positive order tokens

let cutDag_Pos_dotnet (sentence: string) = cutDag_Pos sentence |> List.map (fun (k , v)-> Pair(k, v)) |> Array.ofList

let cutDagWithoutHmm_Pos (sentence : string) =
    let dag = getDag sentence
    let route = calc sentence dag
    let n = sentence.Length
    let rec getTokens x (buf: string) tokens =
        match x < n with
        | false -> if buf.Length > 0 then (buf, "eng") :: tokens else tokens
        | _ ->
            let y = route.[x] |> fst |> (+) 1
            let w = sentence.Substring(x, y - x)
            match RegexEnglishChar.IsMatch w with 
            | true -> getTokens y (buf + w) tokens
            | _ ->
                let wTokens = w, getMapValOrDefault w WordTagTab "x"
                let tempTokens = if buf.Length > 0 then wTokens :: (buf, "eng") :: tokens else wTokens :: tokens
                getTokens y String.Empty tempTokens
    getTokens 0 String.Empty [] //|> List.rev     // reverse the list to get a positive order sequence

let cut_Pos (text : string) (hmm : bool) =
    checkNewUserWordTags()
    let cutMethod = if hmm then cutDag_Pos else cutDagWithoutHmm_Pos
    let getTokens (blk : string) tokens =
        match RegexChineseInternal.IsMatch blk with 
        | true -> cutMethod blk @ tokens                        // reversed order
        | _ ->
            RegexSkipInternal.Split blk |> Array.fold (fun ts x ->
                match RegexSkipInternal.IsMatch x with 
                | true -> (x, "x") :: ts
                | _ -> x |> Seq.fold (fun tts xx -> 
                            let xxs = xx.ToString()
                            let pos = if RegexNumbers.IsMatch xxs then "m" elif RegexEnglishWords.IsMatch x then "eng" else "x"
                            (xxs, pos) :: tts
                        ) ts
                ) tokens                                        // reversed order
    RegexChineseInternal.Split text |> Array.fold (fun ts blk -> getTokens blk ts) [] |> List.rev       // reverse the list to get a positive order sequence

let cut_Pos_dotnet (text, hmm) = cut_Pos text hmm |> List.map (fun (k , v) -> Pair(k, v)) |> Array.ofList