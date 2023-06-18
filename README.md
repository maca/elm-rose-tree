# Rose Tree

This library provides an API for working with generic multiway tree data
structures, also known as rose trees.
Each node of the tree holds a value and can have zero or more children,
children are represented as an Array to simplify path traversing.

The library offers a set of functions for creating, traversing, updating, and
manipulating trees.

Some functions take a path consisting of a list of integers for retrieving,
removing, updating, or inserting a new node before or after the node at the
given path.

Other functions are folds and maps that will walk the whole tree, updating nodes
with a transform function or checking for some condition using a predicate
function.
Folds will begin at the deepest node either from the left or right of the
children collection.

Some path manipulation functions are provided.


## Examples:

```
import RoseTree.Tree as Tree exposing (Tree)
import RoseTree.Path as Path


aTree : Tree Int
aTree =
    Tree.branch 0
        [ Tree.branch 1
            [ Tree.leaf 2
            , Tree.leaf 3
            , Tree.leaf 4
            ]
        , Tree.branch 5
            [ Tree.leaf 6
            , Tree.leaf 7
            , Tree.leaf 8
            ]
        ]


sumAll : Tree Int -> Int
sumAll tree =
    Tree.foldl (Tree.value >> (+)) 0 tree
```


Sum values for all nodes.
```
sumAll aTree == 36
```


Get a node at a path.
```
Tree.get [0,1] aTree == Just (Tree.leaf 3 )
```


Replace a node with a sum of its value and its children values.
```
Tree.updateAt [ 1 ] (\n -> Tree.leaf (sumAll n)) aTree
    == Tree.branch 0
        [ Tree.branch 1
            (
                [ Tree.leaf 2
                , Tree.leaf 3
                , Tree.leaf 4
                ]
            )
        , Tree.leaf 26
        ]
```


Transform a tree of integers to a tree of strings.
```
Tree.mapValues String.fromInt aTree
    == Tree.branch "0"
        [ Tree.branch "1"
            [ Tree.leaf "2"
            , Tree.leaf "3"
            , Tree.leaf "4"
            ]
        , Tree.branch "5"
            [ Tree.leaf "6"
            , Tree.leaf "7"
            , Tree.leaf "8"
            ]
        ]
```

Find the common ancestor for a list of paths.
```
Path.ancestor
  [ [ 0, 1, 0, 1 ]
  , [ 0, 1, 1, 0 ]
  , [ 0, 1, 2, 3 ]
  ]
  == [ 0, 1 ]
```
