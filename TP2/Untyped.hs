module Untyped where

import Control.Monad
import Data.List

import Common

--------------------------------------------------
-- Sección 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
--------------------------------------------------
conversion  :: LamTerm -> Term
conversion lt = conversion' lt []

conversion' :: LamTerm -> [(String, Int)] -> Term
conversion' (App t1 t2) xs = (conversion' t1 xs) :@: (conversion' t2 xs)
conversion' (Abs c t)   xs = Lam ((conversion' t) (add c xs))
conversion' (LVar v)    xs = case null $ lookUp v xs of
                              False  -> Bound (head (lookUp v xs))
                              True   -> Free (Global v)
                              
add :: String -> [(String, Int)] -> [(String, Int)]
add c [] = [(c,0)]
add c ((c',i):xs)
   | c == c'   = (c,0):[(p,q+1) | (p,q)<-xs] 
   | otherwise = (c', i+1) : (add c xs)

lookUp :: String -> [(String, Int)] -> [Int]
lookUp _ [] = []
lookUp c ((c',i):xs)
   | c == c'   = [i]
   | otherwise = lookUp c xs

-------------------------------
-- Sección 3 - Evaluación
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

eval :: [(Name,Value)] -> Term -> Value
eval  nvs t = eval' t (nvs,[])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (t1 :@: t2) xs                = vapp (eval' t1 xs) (eval' t2 xs)
eval' (Lam t) (enviroments, bounds) = VLam (\x -> eval' t (enviroments, x:bounds))
eval' (Free n) xs                   = c where [(_,c)] = [(p,q) | (p,q)<-xs, p==n]
eval' (Bound  i)  xs                = (snd xs) !! i

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote  :: Value -> Term
quote  =  undefined
