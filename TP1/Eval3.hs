module Eval3 (eval) where
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
eval :: Comm -> (ErrHandler, Int)
eval p = evalComm p (initState, 0)

data ErrHandler = ERROR | OK State deriving Show

-- Evalua un comando en un estado dado
evalComm :: Comm -> (State, Int) -> (ErrHandler, Int)
evalComm Skip (xs,i) = (OK xs, i)

evalComm (Let v e) (xs,i) = case eval of
      Nothing -> (ERROR, c)
      Just x  -> (OK (update v x xs), c)
   where (eval, c) = evalIntExp e (xs,i)
      
evalComm (Seq c1 c2) xs = case state of
      ERROR -> (ERROR, c)
      OK st -> evalComm c2 (st, c)
   where (state, c) = evalComm c1 xs
      
evalComm (Cond b c1 c2) (xs, i)
   | (evalBoolExp b (xs,i)) = evalComm c1 (xs,i)
   | otherwise              = evalComm c2 (xs,i)
   
evalComm (While b c) (xs,i) 
   | not(evalBoolExp b (xs,i)) = (OK xs, i)
   | otherwise = case eval of
         ERROR    -> (ERROR, n)
         OK state -> evalComm (While b c) (state, n)
      where (eval, n) = evalComm c (xs,i)
         
-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> (State, Int) -> (Maybe Int, Int)
evalIntExp (Const a) (xs,i)  = (Just (fromInteger a), i)
evalIntExp (Var a) (xs,n)    = (Just (lookfor a xs), n)

evalIntExp (UMinus e) (xs,n) = case eval of
      Nothing -> (Nothing, i+n)
      Just x  -> (Just ((-1)*x), i+n+1)
   where (eval, i) = evalIntExp e (xs,0)
   
evalIntExp (Plus e1 e2) (xs,n) = case eval1 of
      Nothing -> (Nothing, i1+n)
      Just x  -> case eval2 of
         Nothing -> (Nothing, i1+i2+n)
         Just y  -> (Just ((+) x y), i1+i2+n+1)
   where (eval1, i1) = evalIntExp e1 (xs,0)
         (eval2, i2) = evalIntExp e2 (xs,0)
                
evalIntExp (Minus e1 e2) (xs,n) = case eval1 of 
      Nothing -> (Nothing, i1+n)
      Just x  -> case eval2 of
         Nothing -> (Nothing, i1+i2+n)
         Just y  -> (Just ((-) x y), i1+i2+n+1)
   where (eval1, i1) = evalIntExp e1 (xs,0)
         (eval2, i2) = evalIntExp e2 (xs,0)
                              
evalIntExp (Times e1 e2) (xs,n) = case eval1 of
      Nothing -> (Nothing, i1+n)
      Just x  -> case eval2 of
         Nothing -> (Nothing, i1+i2+n)
         Just y  -> (Just ((*) x y), i1+i2+n+1)
   where (eval1, i1) = evalIntExp e1 (xs,0)
         (eval2, i2) = evalIntExp e2 (xs,0)
               
evalIntExp (Div e1 e2) (xs,n) = case eval1 of
      Nothing -> (Nothing, i1+n)
      Just x  -> case eval2 of
         Nothing -> (Nothing, i1+i2+n)
         Just 0  -> (Nothing, i1+i2)
         Just y  -> (Just ((div) x y), i1+i2+n+1)
   where (eval1, i1) =  evalIntExp e1 (xs,0)
         (eval2, i2) =  evalIntExp e2 (xs,0)
                            
-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> (State,Int) -> Bool
evalBoolExp exp xs = case exp of
   BTrue     -> True
   BFalse    -> False
   Eq  e1 e2 -> (evalIntExp e1 xs) == (evalIntExp e2 xs) 
   Lt  e1 e2 -> (evalIntExp e1 xs) < (evalIntExp e2 xs)
   Gt  e1 e2 -> (evalIntExp e1 xs) > (evalIntExp e2 xs)
   And e1 e2 -> (evalBoolExp e1 xs) && (evalBoolExp e2 xs)
   Or  e1 e2 -> (evalBoolExp e1 xs) || (evalBoolExp e2 xs)
   Not e1    -> not (evalBoolExp e1 xs)
