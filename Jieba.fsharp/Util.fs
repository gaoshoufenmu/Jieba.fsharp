module Util

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open System.Collections
open System.Collections.Generic
open Newtonsoft.Json
open Predefine

let fail (reader : JsonReader) token =
  let msg = sprintf "Invalid token '%A', at path '%s'" token reader.Path
  raise <| JsonReaderException(msg)

let read (reader : JsonReader) (t : JsonToken) =
  if reader.TokenType = t then
    let value = reader.Value
    reader.Read() |> ignore
    Some value
  else None

let require<'a> reader v =
  match v with
  | Some o -> (o : obj) :?> 'a
  | None   -> fail reader v

let readProp (reader : JsonReader) (n : string) =
  read reader JsonToken.PropertyName |> Option.map (fun v -> if (v :?> string) <> n then fail reader n else v)

let ($) ts arg =
  match (ts, arg) with
  | (f1, f2, f3), arg -> f1 arg, f2 arg, f3 arg

// writer functions

let writeObject (writer : JsonWriter) f =
  writer.WriteStartObject()
  f()
  writer.WriteEndObject()

type MapConverter() =
  inherit JsonConverter()

  let flags = BindingFlags.Static ||| BindingFlags.NonPublic

  let key (kvp : obj) = kvp.GetType().GetProperty("Key").GetValue(kvp, null)
  let value (kvp : obj) = kvp.GetType().GetProperty("Value").GetValue(kvp, null)

  let readProps (reader : JsonReader) readFun =
    let read, require = read reader, require reader
    read JsonToken.StartObject |> require // start of JSON map object

    if reader.TokenType = JsonToken.PropertyName then
      [|
        while reader.TokenType <> JsonToken.EndObject do
          let element = readFun reader
          yield element
      |]
    else
      Array.empty

  let readKv (serialiser : JsonSerializer) (argTypes : Type array) (reader : JsonReader) =
    if reader.TokenType <> JsonToken.PropertyName then
      failwith "expected property name here"

    let mn   = read reader JsonToken.PropertyName |> require<string> reader
    let key  = System.Convert.ChangeType(mn, argTypes.[0])   // primitive conversion attempt
    let value = serialiser.Deserialize(reader, argTypes.[1])
    reader.Read() |> ignore // consume the value too
    FSharpValue.MakeTuple([|key; value|], FSharpType.MakeTupleType argTypes)

  override x.CanConvert t =
    t.IsGenericType
    && typeof<Map<_,_>>.Equals (t.GetGenericTypeDefinition())

  override x.WriteJson(writer : JsonWriter, o : obj, serialiser : JsonSerializer) =
    if o = null then nullArg "value"
    writeObject writer <| fun () ->
      let kvs = o :?> System.Collections.IEnumerable
      for key, value in kvs |> Seq.cast |> Seq.map (fun kv -> (key kv), (value kv)) do
        writer.WritePropertyName(key.ToString())
        serialiser.Serialize(writer, value)

  override x.ReadJson(reader, objectType, existingValue, serialiser) =
    let read, readProp, req = (read, readProp, require) $ reader
    let argTypes = objectType.GetGenericArguments()

    let tupleType =
      argTypes
      |> FSharpType.MakeTupleType

    let constructedIEnumerableType =
      typeof<IEnumerable<_>>
        .GetGenericTypeDefinition()
        .MakeGenericType(tupleType)

    let kvs = readProps reader (readKv serialiser argTypes)
    read JsonToken.EndObject |> req // we need to consume the object close

    let kvsn = System.Array.CreateInstance(tupleType, kvs.Length)
    System.Array.Copy(kvs, kvsn, kvs.Length)

    let methodInfo = objectType.GetMethod("Create", flags, null, [|constructedIEnumerableType|], null)
    methodInfo.Invoke(null, [|kvsn|])

let jsonDeserialize<'a> str= JsonConvert.DeserializeObject<'a>(str, MapConverter())

let getDefaultProb state ch (probs: Map<'a, Map<'b, double>>) =
    match probs.[state].TryFind ch with
    | Some v -> v
    | None -> MinProb

let composeTuple f s = f, s

let getMapValOrDefault (key : 'a) (map: Map<'a, 'b>) ``default`` =
    match map.TryFind key with
    | None -> ``default``
    | Some v -> v

let getMapKeys (map: Map<'k, 'v>) =
    map |> Map.toList |> List.map fst
