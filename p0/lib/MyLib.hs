module MyLib (zipLong) where

zipLong [] _ = []
zipLong _ [] = []
zipLong xs ys = f xs ys False False where
    f [] [] xs_ended ys_ended             = []
    f xs' [] xs_ended ys_ended            = if not xs_ended then f xs' ys xs_ended True else []
    f [] ys' xs_ended ys_ended            = if not ys_ended then f xs ys' True ys_ended else []
    f (x : xs') (y : ys') xs_ended ys_ended = (x, y) : f xs' ys' xs_ended ys_ended
