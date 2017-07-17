module model
open System
open System.IO
open Util
open Predefine

type Trie (ch: char option, freq: float, ts: (string * float) seq) =
    let mutable _freq = freq
    let getFreq_ (tts: (string*float) seq) = 
        match Seq.length tts with 
        | 1 -> 
            let ft = Seq.head tts
            if ft |> fst |> Seq.length = 1 then snd ft else 0.0
        | _ ->
            0.0

    let mutable _children = 
        ts 
        |> Seq.filter (fun (s, f) -> s.Length > 0)
        |> Seq.groupBy (fun t -> fst t |> Seq.head)
        |> Seq.map (fun (c, tts: (string*float)seq) -> 
            (c, Trie(Some c, getFreq_ tts, tts |> Seq.map (fun (s, f) -> s.Substring(1), f))))
        |> Map.ofSeq

    member this.Char = ch
    member this.Freq with get() = _freq and set v = _freq <- v
    member this.Children with get() = _children and set v = _children <- v

    member this.prefixSearch word =
        let rec getTrie (curTrie: Trie) (curVal: string) =
            if curVal.Length = 0 then Some curTrie
            elif curTrie.Children.ContainsKey curVal.[0] |> not then None
            else getTrie curTrie.Children.[curVal.[0]] (curVal.Substring(1))
        getTrie this word

    member this.nonstrictPrefix word =
        this.prefixSearch word |> Option.isSome

    member this.strictPrefix word =
        let endTrie = this.prefixSearch word
        endTrie.IsSome && endTrie.Value.Freq = 0.0

    member this.contains word =
        let endTrie = this.prefixSearch word 
        endTrie.IsSome && endTrie.Value.Freq > 0.0
    
    member this.getFreq word = 
        let endTrie = this.prefixSearch word
        if endTrie.IsSome then endTrie.Value.Freq else 0.0
    
    // get all words under current Trie node
    member this.getWords () =
        let rec getWords_ (curTrie: Trie) substr =
            seq {
                if curTrie.Children.Count = 0 then yield substr     // leaf node 
                else
                    if curTrie.Freq > 0.0 then yield substr           // although is not a leaf node(has children), but it itself is a valid word
                    yield! curTrie.Children 
                        |> Map.toSeq 
                        |> Seq.collect (fun (c, trie) -> getWords_ trie (substr + c.ToString()))
            }
        getWords_ this String.Empty

    member this.getChildChars prefix =
        let endTrie = this.prefixSearch prefix
        if endTrie.IsNone || endTrie.Value.Children.Count = 0 then [] else getMapKeys endTrie.Value.Children

    //member this.Insert (s:string) freq =
    //    let insert 

type Pair<'a, 'b> (key: 'a, value: 'b) =
    member this.Key = key
    member this.Value = value

    override this.ToString() = "Candidate [Key=" + this.Key.ToString() + ", Freq=" + this.Value.ToString() + "]"

type Token (word: string, startIndex: int, endIndex: int) =
    member this.Word = word
    member this.StartIndex = startIndex
    member this.EndIndex = endIndex

    override this.ToString() = String.Format("[{0}, ({1},{2})]", this.Word, this.StartIndex, this.EndIndex)

type Node (value: char, parent: Node option) =
    member this.Value = value
    member this.Parent = parent

type WordDictionary private () =
    let mutable _trie = Map.empty<string, double>
    let mutable _total = 0.0;

    static let addPair m (x : string, y : double) = 
        match Map.tryFind x m with
        | Some (l) -> m
        | None -> Map.add x y m

    static let lineParse (line : string) = 
        match line.Split(' ') with
        | [|x; y;_|] -> Some(x, double y)
        | _ -> None
    
    static let subSetStrs (input: string) = 
        [1..input.Length - 1] |> List.map(fun x -> input.Substring(0, x))
    
    static let updateDict (tuple_op : (string*double)option) map total =
        match tuple_op with
        | Some (word, freq) -> let nmap = subSetStrs word |> List.fold (fun m x -> addPair m (x, 0.0)) (Map.add word freq map)
                               nmap, total + freq
        | _ -> map, total

    static let loadDict ()= 
        try
            File.ReadLines(Predefine.MainDictFile) |> Seq.map (fun line -> lineParse line)
                |> Seq.fold (fun (map, total) tuple_op -> updateDict tuple_op map total) (Map.empty<string, double>, 0.0)
        with
        | ex -> raise ex

    static let createSingleton () = 
        let dict = new WordDictionary()
        let trie, total = loadDict()
        dict.Trie <- trie
        dict.Total <- total
        dict

    static let instance = lazy (createSingleton())
    static member Instance = instance.Value
    member x.Total with get() = _total and set total = _total <- total
    member x.Trie with get() = _trie and set trie = _trie <- trie
    

    member x.containsWord word = 
        match x.Trie.TryFind word with
        | Some freq -> if freq > 0.0 then true else false
        | _ -> false

    member x.GetFreq word =
        match x.Trie.TryFind word with
        | Some freq -> if freq > 0.0 then freq else 1.0
        | _ -> 1.0

    member x.addWord word freq =
        match x.containsWord word with
        | true -> _total <- _total - x.Trie.[word]
        | _ -> ()

        _trie <- subSetStrs word |> List.fold (fun m x -> addPair m (x, 0.0)) _trie
        _total <- _total + freq

    member x.deleteWord word = x.addWord word 0.0

    member x.SuggestFreq word (segs:string list) = 
        segs |> List.fold (fun f s -> x.GetFreq(s) / _total * f) 1.0 |> (+) 1.0 |> max (x.GetFreq(word))
        
    
                
type Edge =
    {
        Start : string
        End : string
        Weight : double
    }


let d = 0.85

type UndirectWeightGraph (edges : Edge seq) =
    

    member x.Graph = edges 
                        |> Seq.groupBy (fun e -> e.Start) 
                        |> Seq.map (fun (k , es) -> k, es |> Seq.map(fun e -> e))
                        |> Map.ofSeq

let getGraphRank (graph : UndirectWeightGraph) =
    let wsdef = 1.0 / (double graph.Graph.Count)

    let ws, outSum = graph.Graph 
                        |> Map.fold (fun (w: Map<string, double>, o: Map<string, double>) k v -> w.Add(k, wsdef), o.Add(k, v |> Seq.sumBy (fun e -> e.Weight)))
                            (Map.empty<string, double>, Map.empty<string, double>)
    let sortedKeys = getMapKeys graph.Graph |> List.sort
    let newWS = [0..9] |> List.fold (fun (w: Map<string, double>) i -> 
                            sortedKeys |> List.fold (fun wi n 
                                                        -> 
                                                            let ss = graph.Graph.[n]
                                                                        |> Seq.fold (fun s edge -> s + edge.Weight/outSum.[edge.End]*wi.[edge.End]) 0.0
                                                            let mmap = wi |> Map.remove n
                                                            mmap.Add(n, (1.0-d)+ d*ss)
                                        ) w
                        ) ws
    let minRank, maxRank = newWS |> Map.fold (fun (min, max) k v ->  (if v < min then v else min), (if v > max then v else max)) (Double.MaxValue, Double.MinValue)
    newWS |> Map.map (fun k v -> (v - minRank / 10.0) / (maxRank - minRank / 10.0))