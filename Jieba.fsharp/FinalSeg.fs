module FinalSeg

open System
open System.IO
open System.Text.RegularExpressions
open Predefine
open Util

type IFinalSeg = interface
    abstract cut : sentence : string -> string seq
end

let States = ['B';'M';'E';'S']
let EndStates = ['E';'S']
let PrevStatus = ['B', ['E';'S']; 'M',['M';'B'];'S',['S';'E'];'E',['B';'M']] |> Map.ofList
let StartProbs = ['B',-0.26268660809250016;'E',-3.14e+100;'M',-3.14e+100;'S',-1.4652633398537678] |> Map.ofList
let RegexChinese = Regex @"([\u4e00-\u9fd5]+)"
let RegexSkip = Regex @"(\d+\.\d+|[a-zA-Z0-9]+)"

let TransProbs = File.ReadAllText(ProbTransFile) |> jsonDeserialize<Map<char, Map<char, double>>> 
let EmitProbs = File.ReadAllText(ProbEmitFile) |> jsonDeserialize<Map<char, Map<char, double>>> 


let startProbAndPath state ch (vmap: Map<char, double>, pmap: Map<char, char list>)=
    vmap.Add(state, getDefaultProb state ch  EmitProbs + StartProbs.[state]), pmap.Add(state, [state])



let maxProbFromPrev (map: Map<char, double>) ch curState =
    PrevStatus.[curState] |> List.map (fun ps -> map.[ps] + getDefaultProb ps curState TransProbs + getDefaultProb curState ch EmitProbs, ps) |> List.maxBy (fun (prob, prevs) -> prob)

let maxProbLast (map: Map<char, double>) =
    EndStates |> List.map (fun s -> map.[s], s) |> List.maxBy (fun (p, s) -> p)

// 所得到的各segment的边界是倒序的
let rec getBoundaries (pl: char list) (bl: (int * int) list) i b n =
    match pl with
    | [] -> 
        match n < i with
        | true -> (n, i) :: bl
        | _ -> bl
    | h :: t -> 
        let next = i + 1
        match h with
        | 'B' -> getBoundaries t bl next i n
        | 'E' -> getBoundaries t ((b, next):: bl) next b next
        | 'S' -> getBoundaries t ((i, next)::bl) next b next
        | _ -> getBoundaries t bl next b n

// v 是倒序
let rec getProbAndPath (v : Map<char, double> list) (p: Map<char, char list>) chars =
    match chars with
    | [] -> v, p
    | ch :: tail ->
        let nv, newPath = States |> List.fold(fun (vm : Map<char, double>, pm : Map<char, char list>) s -> 
                                                    let prob, state = maxProbFromPrev v.Head ch s
                                                    vm.Add(s, prob), pm.Add(s, p.[state] @ [s])) (Map.empty<char, double>, Map.empty<char, char list>)
        getProbAndPath (nv :: v) newPath tail

let viterbiCut (sentence: string) =
    let vs, ps = States |> List.fold (fun (vmap, pmap) state -> startProbAndPath state sentence.[0] (vmap, pmap)) (Map.empty<char, double>, Map.empty<char, char list>)
    let sentence_list = sentence |> List.ofSeq
    let v, path = sentence_list.Tail |> getProbAndPath [vs] ps
    let prob, state = maxProbLast v.Head    // v是倒序，所以Head其实是最后的状态对应的句子联合概率
    
    // 由于segment边界是倒序排列的，所以经过fold函数后又变成正序的segment
    getBoundaries path.[state] [] 0 0 0 |> List.fold (fun strs (s, e) -> sentence.Substring(s, e - s) :: strs) [] 


let cut (sentence: string) =
    let handle segment =
        match RegexChinese.IsMatch segment with
        | true -> viterbiCut segment
        | _ ->
            RegexSkip.Split segment |> Array.filter (fun s -> not (String.IsNullOrEmpty s)) |> List.ofArray
    RegexChinese.Split sentence |> Array.map (fun segment -> handle segment) |> Array.fold (fun strs segs -> strs @ segs) []

let cut2Arr sentence = cut sentence |> Array.ofList