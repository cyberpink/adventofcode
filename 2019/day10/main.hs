import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Point = P Int Int deriving Show
top (x, y) = P x y
dist (P ax ay) (P bx by) = (abs $ ax - bx) + (abs $ ay - by)
atan2' (P x y) = atan2 (- (fromIntegral x)) (fromIntegral y)
clock (P ox oy) (P hx hy) = P (quot x d) (quot y d)
  where
    (x, y) = (hx - ox, hy - oy) 
    d = case gcd x y of
          0 -> 1
          d -> d
instance (Eq Point) where
  a == b = atan2' a == atan2' b
  a /= b = atan2' a /= atan2' b
instance (Ord Point) where
  compare a b = compare (atan2' a) (atan2' b)
  a <= b = atan2' a <= atan2' b

add a (Just x) = Just (a : x)
add a Nothing = Just [a]
clocks o xs = foldl (\s h -> M.alter (add (top h)) (clock o (top h)) s) M.empty xs
withSize o x = (o, M.size x, x)
pmax a@(_, ac, _) b@(_, bc, _) = if bc > ac then b else a
p1 xs = foldl (\m o -> pmax m (withSize (top o) (clocks (top o) (S.delete o xs)))) ((P 0 0), 0, M.empty) xs
p2 [] o = case o [] of
  [] -> []
  o' -> p2 o' id
p2 ((fc, []) : r) o = []
p2 ((fc, [fn]) : r) o  = fn : p2 r o
p2 ((fc, (fn : fr)) : r) o = fn : p2 r (\t -> o $ (fc, fr) : t)

p s = S.fromList $ do
  (y, l) <- zip [0..] (lines s)
  (x, c) <- zip [0..] l
  guard (c /= '.')
  return (x, y)
main = do
  s <- getContents
  let ((P ox oy), cc, cs) = p1 (p s)
  let (P x y) = (p2 (M.toList $ M.map (sortOn (dist (P ox oy))) cs) id) !! 199
  print (P ox oy, cc)
  print $ x * 100 + y
