#!/usr/bin/env runhaskell

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Expr
  = OpExpr Char Expr Expr
  | NumExpr Int

pExpr :: ReadP Expr
pExpr = pTermExpr `chainl1` pOpExpr
  where
    pOp op = skipSpaces *> char op <* skipSpaces
    pOpExpr = (OpExpr <$> pOp '+') +++ (OpExpr <$> pOp '*')
    pNumExpr = NumExpr . read <$> munch1 isDigit
    pExprInParens = between (char '(') (char ')') pExpr
    pTermExpr = pNumExpr +++ pExprInParens

evalExpr :: Expr -> Int
evalExpr (OpExpr '+' l r) = evalExpr l + evalExpr r
evalExpr (OpExpr '*' l r) = evalExpr l * evalExpr r
evalExpr (NumExpr n)      = n

main :: IO ()
main = interact $ show . sum . map (evalExpr . fst . last . readP_to_S pExpr) . lines
