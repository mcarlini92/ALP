module Eval2 (eval) where
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
eval :: Comm -> ErrHandler
eval p = evalComm p initState

data ErrHandler = ERROR | OK State deriving Show

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> ErrHandler
evalComm Skip xs = OK xs
evalComm (Let v e) xs = case (evalIntExp e xs) of
   Nothing -> ERROR
   Just x -> OK (update v x xs)
evalComm (Seq c1 c2) xs = case (evalComm c1 xs) of
   ERROR -> ERROR
   OK state -> evalComm c2 state
evalComm (Cond b c1 c2) xs
   | (evalBoolExp b xs) = evalComm c1 xs
   | otherwise          = evalComm c2 xs
evalComm (While b c) xs 
   | not(evalBoolExp b xs) = OK xs
   | otherwise = case (evalComm c xs) of
      ERROR    -> ERROR
      OK state -> evalComm (While b c) state

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Maybe Int
evalIntExp (Const a) xs  = Just a
evalIntExp (Var a) xs    = Just (lookfor a xs)
evalIntExp (UMinus a) xs = case (evalIntExp a xs) of
   Nothing -> Nothing
   Just x  -> Just ((-1) * x)
evalIntExp (Plus a b) xs = case (evalIntExp a xs) of
   Nothing -> Nothing
   Just x  -> case (evalIntExp b xs) of
               Nothing -> Nothing
               Just y  -> Just ((+) x y)
evalIntExp (Minus a b) xs = case (evalIntExp a xs) of 
   Nothing -> Nothing
   Just x  -> case (evalIntExp b xs) of
               Nothing -> Nothing
               Just y  -> Just ((-) x y)
evalIntExp (Times a b) xs = case (evalIntExp a xs) of
   Nothing -> Nothing
   Just x  -> case (evalIntExp b xs) of
               Nothing -> Nothing
               Just y  -> Just ((*) x y)
evalIntExp (Div a b) xs = case (evalIntExp a xs) of
   Nothing -> Nothing
   Just x  -> case (evalIntExp b xs) of
               Nothing -> Nothing
               Just 0  -> Nothing
               Just y  -> Just ((div) x y)
               
-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp exp xs = case exp of
	BTrue     -> True
	BFalse    -> False
	Eq  e1 e2 -> (evalIntExp e1 xs) == (evalIntExp e2 xs) 
	Lt  e1 e2 -> (evalIntExp e1 xs) < (evalIntExp e2 xs)
	Gt  e1 e2 -> (evalIntExp e1 xs) > (evalIntExp e2 xs)
	And e1 e2 -> (evalBoolExp e1 xs) && (evalBoolExp e2 xs)
	Or  e1 e2 -> (evalBoolExp e1 xs) || (evalBoolExp e2 xs)
	Not e1    -> not (evalBoolExp e1 xs)