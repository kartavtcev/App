module App.Ui.Console

open App.Core
open ImmutableTrie
open System
open System.Text.RegularExpressions

let count (tree : Trie) =
  let visitor =
    { new IVisit<int> with
          member this.Visit quant word =
            if quant.Value > 0 then
              printfn "'%s' : %d" word quant.Value
    }
  tree.Traverse(visitor.Visit, "")

let concat left right =
    List.append left right

[<EntryPoint>]
let main argv =

  let invalidWordsReg = Regex( "^\d+$|^\d+\w+$|^\w+\d+$|^\d+.+$|\s+|(�|©|@|®)+" )

  let mutable words = []
  let mutable line = " "

  while not (String.IsNullOrEmpty(line)) do
  
    line <- Console.ReadLine()
    if (not (String.IsNullOrEmpty(line))) then
      let rawWords = line.Split(
                      [|" "; "/"; "\\"; ","; "."; ";"; "?"; "!"; ":";")"; "("; "<"; ">"; "["; "]"; "}"; "{"; "+"; "-"; "*"; "%"; "_"; "#"; "&"; "§"; "$"; "~"; "|"; "^"; "\""; "'"; "="; "”"; "“"|],
                      StringSplitOptions.RemoveEmptyEntries)
      let smth = rawWords
                |> Seq.map(fun word -> word.Trim() )
                |> Seq.map(fun word -> word.Replace("”", "").Replace("“", ""))
                |> Seq.map(fun word -> word.ToLowerInvariant())
                |> Seq.filter(fun word -> (invalidWordsReg.IsMatch word ) = false)
                |> Seq.toList

      words <- concat words smth

  //let words = ["hello"; "hi"; "hello"; "helloworld"]
  let trie = Trie.Build(words)
  count(trie)

  Console.Read() |> ignore
  0
