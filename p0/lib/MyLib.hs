module MyLib where

zipLong [] _ = []
zipLong _ [] = []
zipLong xs ys = f xs ys False False where
    f [] [] xs_ended ys_ended             = []
    f xs' [] xs_ended ys_ended            = if not xs_ended then f xs' ys xs_ended True else []
    f [] ys' xs_ended ys_ended            = if not ys_ended then f xs ys' True ys_ended else []
    f (x : xs') (y : ys') xs_ended ys_ended = (x, y) : f xs' ys' xs_ended ys_ended

data Tree a
  = Empty
  | Node
    { left :: Maybe (Tree a)
    , value :: a
    , right :: Maybe (Tree a)
    }
  deriving (Eq,Show,Read)

empty :: Tree a
empty = Empty

leaf :: a -> Tree a
leaf a = Node Nothing a Nothing

traversal :: Tree a -> [a]
traversal Empty = []
traversal (Node ml v mr)
  = maybe [] traversal ml ++ [v] ++ maybe [] traversal mr

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = leaf v
insert v t@(Node ml root mr)
  | v < root  = t{ left = Just $ maybe (leaf v) (insert v) ml }
  | otherwise = t{ right= Just $ maybe (leaf v) (insert v) mr }

rotateLeft :: Tree a -> Tree a
rotateLeft Empty = Empty
rotateLeft (Node l v (Just (Node rl rv rr))) = Node (Just (Node l v rl)) rv rr
rotateLeft t = t

rotateRight :: Tree a -> Tree a
rotateRight Empty = Empty
rotateRight (Node (Just (Node ll lv lr)) v r) = Node ll lv (Just (Node lr v r))
rotateRight t = t

isEmpty Empty = True
isEmpty _ = False