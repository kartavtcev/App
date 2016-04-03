namespace App.Core

module Words =  // probable move to namespace and all other to separate modules

  type Quant<'T> = {
    Character : char;
    Value : 'T;
  }

  type Word = {
    Word : string;
    Count : int;
  }

  type IVisit<'T> =
    abstract member Visit: Quant<'T> -> unit

  type Trie<'T> =
    | Node of Quant<'T> * Trie<'T> array
    | Leaf of Quant<'T>
    with
      member this.Traverse f =
        match this with
        | Node(quant, tries) ->
          f(quant)
          tries |> Seq.iter (fun trie -> trie.Traverse f)
        | Leaf(quant) -> f(quant)

  let count (tree : Trie<int>) =
    let visitor =
      let result = ref []   // implicit mutable here
      ({ new IVisit<int> with
            member this.Visit(quant) =
                result := (!result :: ![quant])
      }, result)

    tree.Traverse (fst visitor).Visit
    !(snd visitor)
