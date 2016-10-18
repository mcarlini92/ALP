module Parser where

import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding ((<|>))

import Common
import Untyped

----------------------------------------------
-- Seccon 2 - Representacion de Lambda Terminos 
-- Ejercicio 1
----------------------------------------------

num :: Integer -> LamTerm
num 0 = Abs "s" (Abs "z" (LVar "z"))
num n = let (Abs _ (Abs _ w)) = num (n-1)
      in (Abs "s" (Abs "z" (App (LVar "s") w)))
-------------------------------------------------
-- Parser de Lambda Calculo (Gramatica Extendida) 
-------------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do 
              whiteSpace untyped
              t <- p
              eof
              return t

-- Analizador de Tokens
untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_'
                                    , opStart    = oneOf "=.\\"
                                    , opLetter = parserZero
                                    , reservedOpNames = ["=",".","\\"]
                                    , reservedNames = ["def"]
                                    })

-- Parser para comandos
parseStmt :: Parser a -> Parser (Stmt a)
parseStmt p = do
         reserved untyped "def"
         x <- identifier untyped
         reservedOp untyped "="
         t <- p
         return (Def x t)
   <|> fmap Eval p


parseTermStmt :: Parser (Stmt Term)
parseTermStmt = fmap (fmap conversion) (parseStmt parseLamTerm)

-- Parser para LamTerms 
parseLamTerm :: Parser LamTerm
parseLamTerm =
   do bs <- many1 parseVarAbs
      return (foldl1 App bs) --foldl1 garantiza la asociacion izquierda de la aplicacion

parseVarAbs :: Parser LamTerm
parseVarAbs = do ide <- identifier untyped
                 return (LVar ide)
--lexeme toma un parser y devuelve otro que consume todos los espacios (tabuladores y  newlines), a la derecha, del token parseado-}
         <|> do x <- lexeme untyped (decimal untyped) 
                return (num (fromInteger x))
         <|> do reservedOp untyped "\\"
                vars <- many1 (identifier untyped)
                reservedOp untyped "."
                lt <- parseLamTerm
                return (foldr Abs lt vars)
         <|> parens untyped parseLamTerm

-- para testear el parser interactivamente.
testParser :: Parser LamTerm
testParser = totParser parseLamTerm