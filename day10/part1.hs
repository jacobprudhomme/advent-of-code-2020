import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List (sort)

joltageDifferences :: [Int] -> [Int]
joltageDifferences ns = zipWith (-) (ns ++ [last ns + 3]) (0:ns)

main :: IO ()
main = interact $ show . uncurry (*) . bimap length length . (filter (==1) &&& filter (==3)) . joltageDifferences . sort . map read . lines
