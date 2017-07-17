module Util

val jsonDeserialize : string -> 'a

val composeTuple: 'a -> 'b -> 'a * 'b

val getDefaultProb : 'a -> 'b -> Map<'a, Map<'b, double>> -> double

val getMapValOrDefault : 'a -> Map<'a, 'b> -> 'b -> 'b

val getMapKeys : Map<'k , 'v> -> 'k list