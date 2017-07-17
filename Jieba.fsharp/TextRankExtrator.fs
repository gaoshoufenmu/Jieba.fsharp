module TextRankExtrator

open System.IO
open System
open Predefine
open Util
open model
open PosSeg


let DefaultPosFilter = 
    ["n";"ng";"nr";"nrfg";"nrt";"ns";"nt";"nz";"v";"vd";"vg";"vi";"vn";"vq"] |> Set.ofList

let DefaultStopwords =
    ["the";"of";"is";"and";"to";"in";"we";"for";"an";"are";"by";"be";"as";"on";"with";"can";"if";
        "from";"which";"you";"it";"this";"then";"at";"have";"all";"not";"one";"has";"or";"that"] |> Set.ofList 

let mutable Span = 5

let Stopwords = 
    match File.Exists (StopwordsFile) with
    | true -> 
        File.ReadAllLines StopwordsFile |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x <> String.Empty) |> Set.ofArray
    | _ -> DefaultStopwords

let pairFilter (word: string, flag: string) =
    let b1 = DefaultPosFilter.Contains flag 
    let b2 = word.Trim().Length >= 2 
    let b3 = not (Stopwords.Contains (word.ToLower()))
    let b4 = b3
    b1 && b2 && b3

let extractTagRank (text: string) (allowPos: string seq) =
    let rec getWords (words : string list) (ts:(string*string) list)  =
        match ts with
        | [] -> words
        | h :: tail ->
            match pairFilter h with 
            | false -> getWords words tail
            | _ ->
                let rec getComposedWord follows i composedWords =
                    if i = Span then composedWords
                    else
                        match follows with 
                        | [] -> composedWords
                        | hh :: tt ->
                            match pairFilter hh with 
                            | false -> getComposedWord tt (i+1) composedWords
                            | _ ->
                                let key = fst h + "$" + fst hh
                                getComposedWord tt (i+1) (key :: composedWords)
                let newWords = getComposedWord tail 1 words
                getWords newWords tail 
    
    let words = cut_Pos text true
    let edges = words
                |> getWords []
                |> Seq.ofList 
                |> Seq.groupBy (fun s -> s)
                |> Seq.fold (fun list (s, seq) -> 
                                let segs = s.Split('$')
                                let edge1 = { Start = segs.[0]; End = segs.[1]; Weight = double (Seq.length seq)}
                                let edge2 = {Start = edge1.End;End = edge1.Start;Weight = edge1.Weight}
                                edge1 :: edge2 :: list) []
                |> Seq.ofList

    UndirectWeightGraph edges |> getGraphRank

let extractTags (text : string) count (allowPos: string seq) =
    let rank = extractTagRank text allowPos
    let pairs = rank |> Map.toList |> List.sortBy (fun (k , v) -> v)
    let newCount = if count <= 0 then 20 else count
    let discardCount = newCount - pairs.Length     // 需要丢弃的数量的相反数
    let rec getKeysByOrderDescending (ps: (string * double) list) i keys =
        match ps with 
        | [] -> keys
        | h :: tail ->
            if i < 0 then getKeysByOrderDescending tail (i+1) keys
            else getKeysByOrderDescending tail 0 (fst h :: keys)
    getKeysByOrderDescending pairs discardCount []

let extractTagWeights (text: string) count (allowPos: string seq) =
    let rank = extractTagRank text allowPos
    let pairs = rank |> Map.toList |> List.sortBy (fun (k , v) -> v)
    let newCount = if count <= 0 then 20 else count
    let discardCount = newCount - pairs.Length     // 需要丢弃的数量的相反数
    let rec getKeysByOrderDescending ps i keys =
        match ps with 
        | [] -> keys
        | h :: tail ->
            if i < 0 then getKeysByOrderDescending tail (i+1) keys
            else getKeysByOrderDescending tail 0 (h :: keys)
    getKeysByOrderDescending pairs discardCount []