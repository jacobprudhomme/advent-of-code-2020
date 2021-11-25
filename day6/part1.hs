import Data.List.Split (splitWhen)
import Data.Set (Set)

import qualified Data.Set as S

collectYesesForGroup :: [String] -> Set Char
collectYesesForGroup = foldr (S.union . S.fromList) S.empty

main :: IO ()
main = interact $ show . sum . map (S.size . collectYesesForGroup) . splitWhen null . lines
