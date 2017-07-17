namespace Jieba.test.fsharp

open System
open System.IO
open NUnit.Framework
open FsUnit
open Predefine
open JiebaSegmenter
open PosSeg
open SpellChecker
open TextRankExtrator
open TfIdfExtractor

[<TestFixture>] 
type ``Test Jieba Segment`` () =

    [<Test>] member x.
     ``Cut All and HMM.`` ()=
        
        cut "我来到北京清华大学" true true |> 
            List.fold (fun (joint: string) seg -> if joint.Length = 0 then seg else String.Format("{0}/{1}", joint, seg) ) String.Empty |> 
                should equal "我/来到/北京/清华/清华大学/华大/大学"

    [<Test>] 
    member x.``Cut With Accurate Mode_1`` () =
        cut "我来到北京清华大学" false true |> 
            List.fold (fun (joint: string) seg -> if joint.Length = 0 then seg else String.Format("{0}/{1}", joint, seg) ) String.Empty |> 
                should equal "我/来到/北京/清华大学"

    [<Test>] 
    member x.``Cut With Accurate Mode_2`` () =
        cut "他来到了网易杭研大厦" false true |> 
            List.fold (fun (joint: string) seg -> if joint.Length = 0 then seg else String.Format("{0},{1}", joint, seg) ) String.Empty |> 
                should equal "他,来到,了,网易,杭研,大厦"

    [<Test>] 
    member x.``Cut With Search Mode`` () =
        cut4Search "小明硕士毕业于中国科学院计算所，后在日本京都大学深造" true |> 
            List.fold (fun (joint: string) seg -> if joint.Length = 0 then seg else String.Format("{0}, {1}", joint, seg) ) String.Empty |> 
                should equal "小明, 硕士, 毕业, 于, 中国, 科学, 学院, 科学院, 中国科学院, 计算, 计算所, ，, 后, 在, 日本, 京都, 大学, 日本京都大学, 深造"

    [<Test>] 
    member x.``Cut With Pos`` () =
        cut_Pos "我爱北京天安门" true |> 
            List.fold (fun (joint: string) t -> if joint.Length = 0 then String.Format("{0},{1}", fst t, snd t) else String.Format("{0}/ {1},{2}", joint, fst t, snd t) ) String.Empty |> 
                should equal "我,r/ 爱,v/ 北京,ns/ 天安门,ns"

    [<Test>] 
    member x.``Spell Check`` () =
        suggests "小不列颠" |> 
            List.fold (fun (joint: string) s -> if joint.Length = 0 then s else String.Format("{0}/ {1}", joint, s) ) String.Empty |> 
                should equal "大不列颠/ 不列颠"

    [<Test>] 
    member x.``TextRank Check`` () =
        extractTags @"此外，公司拟对全资子公司吉林欧亚置业有限公司增资4.3亿元，增资后，吉林欧亚置业注册资本由7000万元增加到5亿元。吉林欧亚置业主要经营范围为房地产开发及百货零售等业务。目前在建吉林欧亚城市商业综合体项目 2013年，实现营业收入0万元，实现净利润-139.13万元。" 20 Seq.empty<string>
            |> List.fold (fun (joint: string) s -> if joint.Length = 0 then s else String.Format("{0}/{1}", joint, s) ) String.Empty |> 
                should equal "吉林/欧亚/置业/实现/收入/增资/子公司/城市/商业/业务/在建/营业/全资/综合体/注册资本/有限公司/零售/百货/开发/经营范围"

    [<Test>] 
    member x.``TfIdf Check`` () =
        extractTags_Tfidf (File.ReadAllText(ArticleFile)) 20 Seq.empty<string>
            |> List.fold (fun (joint: string) s -> if joint.Length = 0 then s else String.Format("{0}/{1}", joint, s) ) String.Empty |> 
                should equal "分词/算法/成词/词数/歧义/划分/句子/缩略语/方案/最少/人名/单独/w1/匹配/中文/问题/w2/概率/用字/自动"

