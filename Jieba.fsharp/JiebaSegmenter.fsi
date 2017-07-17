module JiebaSegmenter

open model

val mutable UserWordTagTab : Map<string, string>

val WordDict : WordDictionary

val internal getDag : string -> (int* int list) list

val calc : string -> (int * int list) list -> Map<int, (int*double)>

val cut : string -> bool -> bool -> string list

val cut_dotnet: (string*bool*bool) -> string []

val cut4Search : string -> bool -> string list

val cut4Search_dotnet : string*bool -> string []

val addWord : string -> double -> string -> unit

val addWord_dotnet : (string*double*string) -> unit

val deleteWord : string -> unit