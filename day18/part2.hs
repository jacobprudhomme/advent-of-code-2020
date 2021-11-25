import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Expr
  = OpExpr Char Expr Expr
  | NumExpr Int

pExpr :: ReadP Expr
pExpr = pMultExpr
  where
    pOp op = skipSpaces *> char op <* skipSpaces
    pNumExpr = NumExpr . read <$> munch1 isDigit
    pExprInParens = between (char '(') (char ')') pExpr
    pTermExpr = pNumExpr +++ pExprInParens
    pMultExpr = pAddExpr `chainl1` (OpExpr <$> pOp '*')
    pAddExpr = pTermExpr `chainl1` (OpExpr <$> pOp '+')

evalExpr :: Expr -> Int
evalExpr (OpExpr '+' l r) = evalExpr l + evalExpr r
evalExpr (OpExpr '*' l r) = evalExpr l * evalExpr r
evalExpr (NumExpr n)      = n

main :: IO ()
main = interact $ show . sum . map (evalExpr . fst . last . readP_to_S pExpr) . lines
