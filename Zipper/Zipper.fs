// Copyright (c) Kk Shinkai. All Rights Reserved. See LICENSE.txt in the project
// root for license information.

namespace KkShinkai.Zipper.BinaryTreeZipper

type 'item tree =
  | Leaf of 'item
  | Node of 'item tree list

type 'item path  =
  | Top
  | At of leftSibling  : 'item tree list
        * parent       : 'item path
        * rightSibling : 'item tree list

type 'item location =
  | Loc of 'item tree * 'item path

module Zipper =
  let walkLeft (Loc (tree, path)): 'a location =
    match path with
    | Top -> failwith "cannot walk to left of the top node"
    | At (l::ls, parent, rs) -> Loc (l, At (ls, parent, tree::rs))
    | At ([], _parent, _rs) -> failwith "cannot walk to left of the first child"

  let walkRight (Loc (tree, path)): 'a location =
    match path with
    | Top -> failwith "cannot walk to right of the top node"
    | At (ls, parent, r::rs) -> Loc (r, At (tree::ls, parent, rs))
    | At (_ls, _parent, []) -> failwith "cannot walk to right of the last child"

  let walkUp (Loc (tree, path)): 'a location =
    match path with
    | Top -> failwith "cannot walk to parent of the top node"
    | At (ls, parent, rs) -> Loc (Node (List.rev ls @ [tree] @ rs), parent)

  let walkDown (Loc (tree, path)): 'a location =
    match tree with
    | Leaf _ -> failwith "cannot walk to child of the leaf"
    | Node (c::cs) -> Loc (c, At ([], path, cs))
    | Node [] -> failwith "cannot walk to child of an empty node"

  let change (Loc (_, path)) tree =
    Loc (tree, path)

  let insertRight (Loc (tree, path)) item =
    match path with
    | Top -> failwith "cannot insert right of the top node"
    | At (ls, parent, rs) -> Loc (tree, At (ls, parent, item::rs))

  let insertLeft (Loc (tree, path)) item =
    match path with
    | Top -> failwith "cannot insert left of the top node"
    | At (ls, parent, rs) -> Loc (tree, At (item::ls, parent, rs))

  let insertDown (Loc (tree, path)) item =
    match tree with
    | Leaf _ -> failwith "cannot insert child to a leaf"
    | Node cs -> Loc (item, At ([], path, cs))

  let delete (Loc (_, path)) =
    match path with
    | Top -> failwith "cannot delete the top node"
    | At (ls, parent, r::rs) -> Loc (r, At (ls, parent, rs))
    | At (l::ls, parent, []) -> Loc (l, At (ls, parent, []))
    | At ([], parent, []) -> Loc (Node [], parent)
