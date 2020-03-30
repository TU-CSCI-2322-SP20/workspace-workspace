--Calculator: 
--Go from "+ 7 3" to 10
--"+ 7 * 2 4" to 7 + (2*4) to 15
--
--First step: Lexing - turning strings (sequences of characters) into sequences of words.
--"+ 173 24" -> ["+", "173", "24"] -> [Oper "Mandy", Number 173, Number 24]

--option 1: to have a different token constructor for each operator
data Token = Number Double | PlusT | SubT | MultT | DivT 

--option 2: to have a sub-type for operator, and a single token constructor
{-data Operator = PlusOp | SubOp | DivOp | MultOp  
data Token = Number Double | Oper Operator-}

lexWord :: String -> Token
--lexWord "273" = Number 273
--lexWord "+" = PlusT
