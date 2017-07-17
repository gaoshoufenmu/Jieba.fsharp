module Predefine

open Config

let MinProb = -3.14e100
let NounPos = ["n"; "ng"; "nr"; "nrfg"; "nrt"; "ns"; "nt"; "nz" ]
let VerbPos = ["v"; "vd"; "vg"; "vi"; "vn"; "vq"]
let IdiomPos = ["i"]

let NounAndVerbPos = NounPos @ VerbPos

let ResourceDir = if IsRefedByFSharp then __SOURCE_DIRECTORY__ + @"\Resources\" else @".\Resources\"

let MainDictFile = ResourceDir + "dict.txt"
let ProbTransFile = ResourceDir + "prob_trans.json"
let ProbEmitFile = ResourceDir + "prob_emit.json"
let PosProbStartFile = ResourceDir + "pos_prob_start.json"
let PosProbTransFile = ResourceDir + "pos_prob_trans.json"
let PosProbEmitFile = ResourceDir + "pos_prob_emit.json"
let CharStateTabFile = ResourceDir + "char_state_tab.json"
let StopwordsFile = ResourceDir + "stopwords.txt"
let IdfFile = ResourceDir + "idf.txt"
let ArticleFile = ResourceDir + "article.txt"
