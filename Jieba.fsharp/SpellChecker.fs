module SpellChecker

open Util
open model

let _Trie_, FirstChars = 
    let tuples = WordDictionary.Instance.Trie |> Map.toSeq |> Seq.filter (fun (s, f) -> f > 0.0)
    let firstChars = tuples 
                            |> Seq.filter (fun (s, f) -> s.Length >= 2)
                            |> Seq.groupBy (fun (s, f) -> s.[1])
                            |> Seq.map (fun (c , ts) -> (c, ts |> Seq.map (fun (s , f) -> s.[0]) |> Set.ofSeq))
                            |> Map.ofSeq
    Trie(None, 0.0, tuples), firstChars

let getEdits1 (word:string) =
    let splits = Set.ofSeq [0..word.Length - 1] |> Set.map (fun i -> word.Substring(0, i), word.Substring(i))
    let deletes = splits |> Set.map (fun (l , r) -> l + r.Substring(1))    // delete a char
    let transposes = splits                                                 // transpose two adjacent chars once
                        |> Set.filter (fun (l , r) -> r.Length > 1)
                        |> Set.map (fun (l , r) -> l + r.[1].ToString() + r.[0].ToString() + r.Substring(2))
    let replaces = 
            match word.Length > 1 with
            | false -> Set.empty<string>
            | _ ->
                let reps = FirstChars.[word.[1]] 
                            |> Set.fold (fun (strs: string Set) first -> 
                                                if first <> word.[0] then strs.Add(first.ToString() + word.Substring(1))else strs) Set.empty<string> // according to snd char, replace first char
                let rec replaceEachChar i (node_op : Trie option) (repls: string Set) =
                    match i < word.Length && node_op.IsSome && node_op.Value.Children.Count > 0 with
                    | false -> repls
                    | _ ->
                        getMapKeys node_op.Value.Children 
                            |> List.fold (fun (rs: string Set) k -> rs.Add(word.Substring(0, i) + k.ToString() + word.Substring(i+1))) repls
                            |> replaceEachChar (i+1) (node_op.Value.Children.TryFind word.[i])
                replaceEachChar 1 (_Trie_.Children.TryFind word.[0]) reps

    let inserts =
            match word.Length > 1 with
            | false -> Set.empty<string>
            | _ ->
                let ins = 
                    match FirstChars.TryFind word.[0] with
                    | None -> Set.empty<string>
                    | Some s -> s |> Set.fold (fun is c -> is.Add(c.ToString() + word)) Set.empty<string>
                let rec insertEachChar i (node_op : Trie option) (insrts: string Set) =
                    match i < word.Length && node_op.IsSome && node_op.Value.Children.Count > 0 with
                    | false -> insrts
                    | _ ->
                        let newIns = getMapKeys node_op.Value.Children 
                                        |> List.fold (fun (rs: string Set) k -> rs.Add(word.Substring(0, i+1) + k.ToString() + word.Substring(i+1))) insrts
                        match i >= word.Length - 1 with
                        | true -> newIns
                        | _ -> 
                            insertEachChar (i+1) (node_op.Value.Children.TryFind word.[i]) newIns
                insertEachChar 0 (_Trie_.Children.TryFind word.[0]) ins

    deletes |> Set.union transposes |> Set.union replaces |> Set.union inserts
                    

let suggests word =
    if WordDictionary.Instance.containsWord word then [word]
    else
        let edits = getEdits1 word
        let candicates = edits |> Set.filter (fun s -> WordDictionary.Instance.containsWord s)
        if candicates.Count > 0 then candicates |> Set.toList |> List.sortBy (fun s -> WordDictionary.Instance.GetFreq s)
        else
            edits 
                |> Set.fold (fun (set: string Set) e -> getEdits1 e |> Set.filter (fun s -> WordDictionary.Instance.containsWord s) |> Set.union set) Set.empty<string>
                |> Set.toList
                |> List.sortBy (fun s -> WordDictionary.Instance.GetFreq s)