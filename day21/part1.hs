#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , containers ^>= 0.6.2.1
             , split ^>= 0.2.3.4
-}

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Set (Set, (\\))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Ingredient = String
type Allergen = String

-- Set intersection drops any ingredients that cannot contain the allergen, because the ingredient
-- containing that allergen would have to be in every single food containing that allergen
potentialIngredientsContainingAllergen :: [([Ingredient],[Allergen])] -> ([([Ingredient],[Allergen])], Map Allergen (Set Ingredient))
potentialIngredientsContainingAllergen = id &&& (M.unionsWith S.intersection . map (uncurry insertAllergensForIngredients))
  where
    insertAllergensForIngredients ingredients allergens =
      let ingredientsSet = S.fromList ingredients
      in foldr (\allergen acc -> M.insert allergen ingredientsSet acc) M.empty allergens

determineIngredientsContainingNoAllergens :: [([Ingredient],[Allergen])] -> Map Allergen (Set Ingredient) -> Set Ingredient
determineIngredientsContainingNoAllergens ingredientsAndAllergens allergensToPotentialIngredients =
  let allIngredients = S.unions (map (S.fromList . fst) ingredientsAndAllergens)
      ingredientsPotentiallyContainingAllergen = S.unions (M.elems allergensToPotentialIngredients)
  in allIngredients \\ ingredientsPotentiallyContainingAllergen

countIngredientsContainingNoAllergens :: [([Ingredient],[Allergen])] -> Int
countIngredientsContainingNoAllergens ingredientsAndAllergens =
  let allIngredients = foldr (\(ingredients,_) acc -> ingredients ++ acc) [] ingredientsAndAllergens
      ingredientsContainingNoAllergens = uncurry determineIngredientsContainingNoAllergens (potentialIngredientsContainingAllergen ingredientsAndAllergens)
  in foldr (\ingredient acc -> length (filter (==ingredient) allIngredients) + acc) 0 ingredientsContainingNoAllergens

splitIngredientsAndAllergens :: String -> ([Ingredient],[Allergen])
splitIngredientsAndAllergens = stringsToLists . splitOn " (contains "
  where stringsToLists [ingredients,allergens] = (words ingredients, splitOn ", " (init allergens))

main :: IO ()
main = interact $ show . countIngredientsContainingNoAllergens . map splitIngredientsAndAllergens . lines
