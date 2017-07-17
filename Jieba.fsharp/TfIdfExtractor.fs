module TfIdfExtractor

open System
open System.IO
open System.Text
open Predefine
open Util
open JiebaSegmenter
open PosSeg
open TextRankExtrator

let DefaultWordCount = 20

let IdfFreq, MedianIdf =
    let tuples = File.ReadAllLines(IdfFile, Encoding.UTF8)
                    |> Array.fold (fun ts line ->
                        let parts = line.Trim().Split(' ')
                        (parts.[0], double parts.[1]) :: ts) []
    let ordered = tuples |> List.sortBy (fun (f , s) -> s) |> Array.ofList
    tuples |> Map.ofList, ordered.[tuples.Length/2] |> snd

let filterCutByPos (text: string) (allowPos: string seq) =
    cut_Pos text true |> List.filter (fun (s, p) -> allowPos |> Seq.exists (fun pos -> pos = p)) |> List.map (fun (s, p) -> s)

let getWordTfidf (text: string) (allowPos : string seq) =
    let words = if allowPos |> Seq.length = 0 then cut text false true else filterCutByPos text allowPos
    let freqs = words 
                |> List.filter (fun w -> (String.IsNullOrEmpty(w) |> not) && w.Trim().Length > 1 && (Stopwords.Contains(w.ToLower()) |> not))
                |> List.fold (fun (map: Map<string, double>) w ->
                                    match map.TryFind w with 
                                    | Some v ->
                                                let mmap = map.Remove w
                                                mmap.Add(w, v + 1.0)
                                    | None -> map.Add(w, 1.0)
                            ) Map.empty<string, double>
    let total = freqs |> Map.fold (fun sum k v -> sum + v) 0.0
    freqs |> Map.map (fun k v -> getMapValOrDefault k IdfFreq MedianIdf * v / total) |> Map.toList

let extractTags_Tfidf (text: string) count (allowPos: string seq) =
    let newCount = if count <= 0 then DefaultWordCount else count
    let pairs = getWordTfidf text allowPos 
                |> List.sortBy (fun (k, v) -> v)

    let discardCount = newCount - pairs.Length     // 需要丢弃的数量的相反数
    let rec getKeysByOrderDescending (ps: (string * double) list) i keys =
        match ps with 
        | [] -> keys
        | h :: tail ->
            if i < 0 then getKeysByOrderDescending tail (i+1) keys
            else getKeysByOrderDescending tail 0 (fst h :: keys)
    getKeysByOrderDescending pairs discardCount []

let extractTagWeights_Tfidf (text: string) count (allowPos : string seq) =
    let newCount = if count <= 0 then DefaultWordCount else count
    let pairs = getWordTfidf text allowPos 
                |> List.sortBy (fun (k, v) -> v)
    let discardCount = newCount - pairs.Length     // 需要丢弃的数量的相反数
    let rec getKeysByOrderDescending ps i keys =
        match ps with 
        | [] -> keys
        | h :: tail ->
            if i < 0 then getKeysByOrderDescending tail (i+1) keys
            else getKeysByOrderDescending tail 0 (h :: keys)
    getKeysByOrderDescending pairs discardCount []