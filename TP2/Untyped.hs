module Untyped where

import Control.Monad
import Data.List

import Common

--------------------------------------------------
-- Seccion 2 - Representacion de Terminos Lambda 
-- Ejercicio 2: Conversion de Terminos
--------------------------------------------------
conversion :: LamTerm -> Term
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
-- Seccion 3 - Evaluacion
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) p     = f p 
vapp (VNeutral p) q = VNeutral (NApp p q)

eval :: [(Name,Value)] -> Term -> Value
eval nvs t = eval' t (nvs,[])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound  p) (_,zs) = zs !! p
eval' (Free i) (ys,_)   = head $ [ q | (p,q)<-ys, p==i]
eval' (p :@: q) xs      = vapp (eval' p xs) (eval' q xs)
eval' (Lam p) (ys,zs)   = VLam (\x -> eval' p (ys,x:zs))

--------------------------------
-- Seccion 4 - Mostrando Valores
--------------------------------

quote  :: Value -> Term
quote v = quote' v 0

quote'  :: Value -> Int -> Term
quote' (VLam f) i              = Lam (quote' (f (VNeutral (NFree (Quote i)))) (i+1))
quote' (VNeutral (NFree v)) i  = case v of 
                                    Global xs -> Free (Global xs)
                                    Quote k   -> Bound (i-k-1)
quote' (VNeutral (NApp n v)) i = (quote' (VNeutral n) i) :@: (quote' v i)



