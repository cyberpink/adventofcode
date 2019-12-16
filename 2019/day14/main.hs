{-# LANGUAGE TupleSections #-}
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Text.ParserCombinators.ReadP
import Data.Char

parse :: String -> [([(Int, String)], (Int, String))]
parse s = fst $ last $ readP_to_S (sepBy line (char '\n')) s
 where
  line = (,) <$> sepBy1 elem (string ", ") <* string " => " <*> elem
  elem = (,) <$> readS_to_P reads <* char ' ' <*> many1 (satisfy isAsciiUpper)

p1 rules have [] ore = ore
p1 rules have (("ORE", amount) : r) ore = p1 rules have r (ore + amount)
p1 rules have ((name, amount) : r) ore =
  if owned' < 0
  then p1 rules (M.insert name (owned' + added) have) ((M.toList reqs') ++ r) ore
  else p1 rules (M.insert name owned' have) r ore
  where
    owned = have M.! name
    (produced, reqs) = rules M.! name
    owned' = owned - amount
    needed = -owned'
    (_s, rem) = quotRem needed produced
    s = _s + (if rem > 0 then 1 else 0)
    (added, reqs') = (produced * s, M.map (* s) reqs)

setup rs =
  (M.fromList $ map (\(vs, (s, n)) -> (n, (s, M.fromList $ map (\(s, n) -> (n, s)) vs))) rs,
  M.fromList $ map (\(_, (_, n)) -> (n, 0)) rs)

main = do
  l <- parse <$> getContents
  let (rules, have) = setup l
  print $ p1 rules have [("FUEL", 1)] 0
