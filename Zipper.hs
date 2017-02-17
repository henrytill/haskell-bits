module Zipper where

data Tree a
  = Item a
  | Section [Tree a]
  deriving Show

data Path a
  = Top
  | Node [Tree a] (Path a) [Tree a]
  deriving Show

data Location a = Loc (Tree a) (Path a) deriving Show

example :: Tree String
example =
  Section [ Section [Item "a", Item "*", Item "b"]
          , Item "+"
          , Section [Item "c", Item "*", Item "d"]
          ]

secondMultLoc :: Location String
secondMultLoc =
  Loc (Item "*")
      (Node [Item "c"]
            (Node [Item "+", Section [Item "a", Item "*", Item "b"]]
                  Top
                  [])
            [Item "d"])


goLeft (Loc t p) = case p of
  Top                    -> error "left of top"
  Node (l:left) up right -> Loc l (Node left up (t:right))
  Node [] up right       -> error "left of first"

goRight (Loc t p) = case p of
  Top                    -> error "right of top"
  Node left up (r:right) -> Loc r (Node (t:left) up right)
  _                      -> error "right of last"

goUp (Loc t p) = case p of
  Top                -> error "up of top"
  Node left up right -> Loc (Section (reverse left ++ (t:right))) up

goDown (Loc t p) = case t of
  Item _             -> error "down of item"
  Section (t1:trees) -> Loc t1 (Node [] p trees)
  _                  -> error "down of empty"

top :: Location String
top =
  Loc (Section [ Section [Item "a", Item "*", Item "b"]
               , Item "+"
               , Section [Item "c", Item "*", Item "d"]
               ])
      Top
