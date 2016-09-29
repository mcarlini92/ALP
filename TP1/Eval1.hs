module Eval1 (eval) where
import AST

-- Estados
-- Variable :: String
type State = [(Variable, Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor _ [] = error "Variable no encontrada"
lookfor v ((v',s'):xs) 
   | v == v'   = s'
   | otherwise = lookfor v xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v s [] = [(v,s)]
update v s ((v',s'):xs)
   | v == v'   = [(v,s)] ++ xs
   | otherwise = [(v',s')] ++ (update v s xs)

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> State
evalComm comm xs = case comm of
   Skip         -> xs
   Let    a b   -> update a (evalIntExp b xs) xs
   Seq    a b   -> evalComm b (evalComm a xs)
   Cond   a b c -> if evalBoolExp a xs then evalComm b xs else evalComm c xs
   While  a b   -> if evalBoolExp a xs then evalComm (Seq b d) xs else xs where d = While a b
   
-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Int
evalIntExp exp xs = case exp of
   Const  a     -> (fromInteger a)
   Var    a     -> lookfor a xs
   UMinus a     -> -(evalIntExp a xs)
   Plus   a b   -> (evalIntExp a xs) + (evalIntExp b xs)
   Minus  a b   -> (evalIntExp a xs) - (evalIntExp b xs)
   Times  a b   -> (evalIntExp a xs) * (evalIntExp b xs)
   Div    a b   -> (evalIntExp a xs) `div` (evalIntExp b xs)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp exp xs = case exp of
   BTrue   -> True
   BFalse  -> False
   Eq  a b -> (evalIntExp a xs) == (evalIntExp b xs) 
   Lt  a b -> (evalIntExp a xs) < (evalIntExp b xs)
   Gt  a b -> (evalIntExp a xs) > (evalIntExp b xs)
   And a b -> (evalBoolExp a xs) && (evalBoolExp b xs)
   Or  a b -> (evalBoolExp a xs) || (evalBoolExp b xs)
   Not a   -> not (evalBoolExp a xs)
