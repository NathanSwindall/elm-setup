module PlayGround.P3_ListZipper exposing (..)

--import PlayGround.P3_ListZipper exposing (..)

type Tree a = Empty | Node a (Tree a) (Tree a)


freeTree : Tree Char 
freeTree = 
    Node 'P'
        (Node '0' 
            (Node 'L'
               (Node 'N' Empty Empty)
               (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

changeToP: Tree Char -> Maybe (Tree Char)
changeToP t = 
    case t of 
        (Node x l (Node y (Node _ m n) r)) -> Just (Node x l (Node y (Node 'P' m n) r))
        _ -> Nothing