import Debug.Trace
--Calculator: 
--Go from "+ 7 3" to 10
--"+ 7 * 2 4" to 7 + (2*4) to 15
--
--First step: Lexing - turning strings (sequences of characters) into sequences of words (tokens).
--"+ 173 24" -> ["+", "173", "24"] -> [PlusT, Number 173, Number 24]
--lexer :: String -> [Token]
--
--Second step: Parsing - turning token streams into sentenes (expression)
--parser :: [Token] -> Expr
--
--Third Step: evaluation - turning expressions into values.
--eval :: Expr -> Value



--option 1: to have a different token constructor for each operator
--data Token = NumT Double | PlusT | SubT | MultT | DivT deriving Show

--option 2: to have a sub-type for operator, and a single token constructor
data Operator = PlusOp | SubOp | DivOp | MultOp deriving (Show, Eq)
data Token = NumT Double | OpT Operator deriving (Show, Eq)

data Expr = NumE Double | OpE Operator Expr Expr deriving Show

size :: Expr -> Int
size (NumE _) = 1
size (OpE op l r) = 1 + (size l) + (size r)

type Value = Double

lexWord :: String -> Maybe Token
lexWord "+" = Just $ OpT PlusOp
lexWord "-" = Just $ OpT SubOp
lexWord "*" = Just $ OpT MultOp
lexWord "/" = Just $ OpT DivOp
lexWord x = if all (`elem` "0123456789.") x
            then Just $ NumT (read x :: Double)
            else Nothing

lexer :: String -> Maybe [Token]
lexer str = let mtoks = map lexWord (words str)
            in sequence mtoks
-- 5 * 0
-- 0
--
--data List = Empty | Cons Int List

expr0,expr1,expr2,expr3 :: Expr
toks0 = [NumT 4]
expr0 = NumE 4

toks1 = [OpT PlusOp, NumT 4, NumT 7]
expr1 = OpE PlusOp (NumE 4) (NumE 7)
--   +
-- 7   -
--    3 2
--
toks2 = [OpT PlusOp, NumT 7.0, OpT SubOp, NumT 3.0, NumT 2.0]
expr2 = OpE PlusOp (NumE 7.0) 
                   (OpE SubOp (NumE 3.0) 
                              (NumE 2.0)
                   )

-- Turn - + 7 3 2 into
--    -
--  +   2
-- 7 3

toks3 = [OpT SubOp, OpT PlusOp, NumT 7.0, NumT 3.0, NumT 2.0]
expr3 = OpE SubOp (OpE PlusOp (NumE 7.0) 
                              (NumE 3.0)
                  )
                  (NumE 2)

--data Maybe a = Just a | Nothing

parse :: [Token] -> Maybe Expr
parse toks = fmap fst $ aux toks
  {-case aux toks of
                Just (expr, leftover) -> Just expr
                Nothing -> Nothing-}
  where aux :: [Token] -> Maybe (Expr, [Token])
        aux [] = Nothing
        aux (NumT x:ts) = Just (NumE x, ts)
        aux (OpT op:ts) = 
          case aux ts of
              Nothing -> Nothing
              Just (leftE, leftOver) -> 
                  case aux leftOver of
                      Nothing -> Nothing
                      Just (rightE, leftLeftOver) -> 
                        let finalE =  OpE op leftE rightE 
                        in Just (finalE, leftLeftOver)


readExpr :: String -> Maybe Expr
readExpr str =  --fmap parse (lexer str)
  case lexer str of
      Just toks -> parse toks
      Nothing -> Nothing
{- verbose version 
parse :: [Token] -> Expr
parse toks = fst $ aux toks
  where aux :: [Token] -> (Expr, [Token])
        aux (NumT x:ts) = traceShowId (NumE x, ts)
        aux (OpT op:ts) = 
          let (leftE, leftOver) = aux ts
              (rightE, leftLeftOver) = aux leftOver
              finalE =  OpE op leftE rightE
          in trace ("Saw " ++ show op) $
             trace ("Reading left from " ++ show ts) $
             trace ("Reading right from " ++ show leftOver) $
             trace ("Returning " ++ show (finalE, leftLeftOver)) $
           (finalE, leftLeftOver)
           -}

eval :: Expr -> Value -- value is a Double
eval (NumE val) = val
eval (OpE PlusOp e1 e2) = (eval e1) + (eval e2)
eval (OpE SubOp e1 e2) = (eval e1) - (eval e2)
eval (OpE MultOp e1 e2) = (eval e1) * (eval e2)
eval (OpE DivOp e1 e2) = (eval e1) / (eval e2)

eval2 :: Expr -> Value -- value is a Double
eval2 (NumE val) = val
eval2 (OpE oper e1 e2) = (evalOp oper) (eval e1) (eval e2)

evalOp :: Operator -> (Double -> Double -> Double)
evalOp PlusOp = (+)
evalOp SubOp = (-)
evalOp MultOp = (*)
evalOp DivOp = (/)
