module Data.Taskell.Seq where

import Data.Sequence (Seq, (!?), insertAt, deleteAt)

extract :: Int -> Seq a -> Maybe (Seq a, a)
extract i xs = do
    c <- xs !? i
    let a = deleteAt i xs
    return (a, c)

shiftBy :: Int -> Int -> Seq a  -> Maybe (Seq a)
shiftBy from dir xs | from == 0 && dir < 0 = Nothing
                    | otherwise = do
                        current <- xs !? from
                        let r = deleteAt from xs
                        return $ insertAt (from + dir) current r
