namespace App.Core

module ImmutableTrie =  // probably move to namespace and all other to separate modules
  let (^) l r = sprintf "%s%c" l r

  type Quant<'T> = {
    Character : char;
    Value : 'T;
  }

  type IVisit<'T> =
    abstract member Visit : Quant<'T> -> string -> unit

  type Childs = Trie list
  and Node = Quant<int> * Childs
  and Trie =
    | Root of Childs
    | Node of Node
      member this.Traverse (f : Quant<int> -> string -> unit, path : string) =
        match this with
        | Node(quant, tries) ->
          let word = path ^ quant.Character
          f quant word
          tries |> Seq.iter (fun trie -> trie.Traverse(f, word))
        | Root(tries) ->
          tries |> Seq.iter (fun trie -> trie.Traverse(f, ""))

      static member Build (words: list<string>) : Trie =
        let immutableBuilder = fun x -> x
        let insert(start : Trie, key : string) =

          let updateExistingTrieInList(character:char, trieNew:Trie, tries: Trie list) = [
            for trie in tries do
              match trie with
              | Node(quant, tries) -> yield if quant.Character = character then trieNew else trie
              | _ ->  ()]

          let getNodes(tries : Trie list, predicate : Node -> bool) =
            let nodes = [
              for trie in tries do
                match trie with
                | Node(quant, tries) -> if predicate((quant, tries)) then yield Node(quant, tries)
                | _ -> ()]
            nodes

          let isPredicate(tries : Trie list,  predicate : Node -> bool) =
            let preds =
              [for trie in tries do
                yield match trie with
                      | Node(quant, tries) -> predicate (quant, tries)
                      | _ -> false ]
            List.fold (||) false preds

          let rec inner index current withNode =
            let ch = key.[index]

            match current with
            | Root(tries) ->
              if index = 0 then
                if isPredicate(tries, (fun (t : Node) -> (fst t).Character = ch)) then
                  let node = getNodes(tries, (fun (t : Node) -> (fst t).Character = ch)) |> Seq.exactlyOne
                  let nodeNew =  inner (index + 1) node immutableBuilder    // same index, because root
                  let triesNew = updateExistingTrieInList(ch, nodeNew, tries)
                  withNode(Root triesNew)
                else
                  let node = Node({ Character = ch; Value = 0 }, [])  // placeholder empty node
                  withNode(Root((inner (index + 1) node immutableBuilder) :: tries)) // same index, because root
              else failwith "index > 0 must have Nodes only"
            | Node(quant, tries) ->
              if index = key.Length - 1 then
                withNode (Node({ Character = quant.Character; Value = quant.Value + 1 }, tries))
              else
                if isPredicate(tries, (fun (t : Node) -> (fst t).Character = ch)) then
                  let node = getNodes(tries, (fun (t : Node) -> (fst t).Character = ch)) |> Seq.exactlyOne
                  let nodeNew =  inner (index + 1) (node) immutableBuilder
                  let triesNew = updateExistingTrieInList(ch, nodeNew , tries)
                  withNode (Node(quant, triesNew))
                else
                  let node = Node({ Character = ch; Value = 0 }, [])  // placeholder empty node
                  withNode (Node({ Character = quant.Character; Value = quant.Value}, (inner (index + 1) node immutableBuilder) :: tries))
          inner 0 start immutableBuilder

        let rec triesCombine(words: list<string>, trie : Trie, withNode: Trie -> Trie) =
            match words with
                        | [] -> withNode(trie)  // check it
                        | lst :: tl ->
                          let node = triesCombine(tl, insert(trie, lst), immutableBuilder)
                          withNode(node)
        triesCombine(words, Root([]), immutableBuilder)

(*
        let mutable trie = Root([])
        for word in words do
          trie <- insert(trie, word)  // mutable way seems to be more clear to read & performance is better
        trie
*)
