module PosSeg

open model

val cutDag_Pos : string -> (string* string) list

val cutDag_Pos_dotnet : string -> Pair<string, string>[]

val cut_Pos : string -> bool -> (string * string) list

val cut_Pos_dotnet : string * bool -> Pair<string, string>[]
