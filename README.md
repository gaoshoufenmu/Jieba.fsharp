# Jieba.fsharp
Jieba segmentation

## Thanks to: 
1) https://github.com/fxsjy/jieba
2) https://github.com/anderscui/jieba.NET

## Notes:
1) All resource files are referenced with fsharp mode by default. If you want to reference this Jieba.fsharp.dll from other non-fsharp .net project, please set 'IsRefedByFSharp = false' (in Module 'Config') first, and copy the '.\Resources' directory into your main program directory
2) All sentence-cutting methods are provided with their corresponding non-fsharp .net versions, which are ended with 'dotnet'. For example, method 'cut' method in module 'JiebaSegmenter' has its non-fsharp partner 'cut_dotnet'

## Notice:
Only those methods used in module 'SegmentTest' are test-passed, other methods are not validity assuring. 
Beg for a forgiveness if codes(or structures) isn't elegant.

