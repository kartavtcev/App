module App.Ui.Console

open App.Core
open Trie

let count (tree : Trie) =
  let visitor =
    { new IVisit<int> with
          member this.Visit quant word =
            if quant.Value > 0 then
              printfn "'%s' : %d" word quant.Value
    }
  tree.Traverse(visitor.Visit, "")

[<EntryPoint>]
let main argv =

  let words = ["hello"; "hi"; "hello"; "helloworld"]
  let trie = Trie.Build(words)
  count(trie)

  System.Console.Read() |> ignore
  0
