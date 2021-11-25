import Data.List (intercalate, sortOn)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Set (Set, (\\))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Ingredient = String
type Allergen = String

-- Set intersection drops any ingredients that cannot contain the allergen, because the ingredient
-- containing that allergen would have to be in every single food containing that allergen
potentialIngredientsContainingAllergen :: [([Ingredient],[Allergen])] -> Map Allergen (Set Ingredient)
potentialIngredientsContainingAllergen = M.unionsWith S.intersection . map (uncurry insertAllergensForIngredients)
  where
    insertAllergensForIngredients ingredients allergens =
      let ingredientsSet = S.fromList ingredients
      in foldr (\allergen acc -> M.insert allergen ingredientsSet acc) M.empty allergens

uniquelyDetermineIngredientsContainingAllergen :: [([Ingredient],[Allergen])] -> [(Ingredient,Allergen)]
uniquelyDetermineIngredientsContainingAllergen = sortOn fst . flip go S.empty . sortOn (S.size . snd) . M.toList . potentialIngredientsContainingAllergen
  where
    go [] _ = []
    go (aAI@(allergen,ingredients):allergensAndIngredients) seenIngredients =
      let potentialIngredients = S.toList (ingredients \\ seenIngredients)
      in if length potentialIngredients == 1
        then (allergen, head potentialIngredients) : go allergensAndIngredients (S.insert (head potentialIngredients) seenIngredients)
        else go (allergensAndIngredients ++ [aAI]) seenIngredients


splitIngredientsAndAllergens :: String -> ([Ingredient],[Allergen])
splitIngredientsAndAllergens = stringsToLists . splitOn " (contains "
  where stringsToLists [ingredients,allergens] = (words ingredients, splitOn ", " (init allergens))

main :: IO ()
main = interact $ show . intercalate "," . map snd . uniquelyDetermineIngredientsContainingAllergen . map splitIngredientsAndAllergens . lines
