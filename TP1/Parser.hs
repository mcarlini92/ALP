module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Char
import AST

----------------------------------------------------
--- Funcion para facilitar el testing del parser ---
----------------------------------------------------
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart    = "/*"
                                  , commentEnd      = "*/"
                                  , commentLine     = "//"
                                  , opLetter        = char '='
                                  , reservedOpNames = ["+","-","*","/","?",">","<","&","|","?"]
                                  , reservedNames   = ["true","false","skip","if","repeat","until",
                                                        "then","else","end", "while","do"]
                                  })
                                  
--------------------------------------
--- Parser de expressiones enteras ---
--------------------------------------
{-
IntExpr   -> IntExpr('+' IntTerm | '-' IntTerm) | IntTerm
IntTerm   -> IntTerm('*' IntFactor | '/' IntFactor) | IntFactor
IntFactor -> <nat> | <var> | '-' <nat> | '(' IntExpr ')'
             | BoolExpr '?' IntExpr ':' IntExpr          
-}

intExp :: Parser IntExp
intExp = chainl1 intTerm addop

intTerm :: Parser IntExp
intTerm = chainl1 intFactor mulop

intFactor :: Parser IntExp
intFactor = do {d<-natural lis; return (Const d)}
            <|> do {reservedOp lis "-"; e<-intFactor; return (UMinus e)}
            <|> do {reservedOp lis "("; e<-intExp; reservedOp lis ")"; return e}
            <|> do {e<-identifier lis; return (Var e)}

addop :: Parser (IntExp -> IntExp -> IntExp)
addop = do {reservedOp lis "+"; return Plus}
        <|> do {reservedOp lis "-"; return Minus}
        
mulop :: Parser (IntExp -> IntExp -> IntExp)
mulop = do {reservedOp lis "*"; return Times}
        <|> do {reservedOp lis "/"; return Div}
               
----------------------------------------
--- Parser de expressiones booleanas ---
----------------------------------------
{-
BoolExpr   -> BoolTerm '|' BoolExpr | BoolTerm
BoolTerm   -> BoolFactor '&' BoolTerm | BoolFactor
BoolFactor -> IntExpr( '=' IntExpr | '<' IntExpr | '>' IntExpr)
              | '¬' BoolExpr | '(' BoolExpr ')' | BTrue | BFalse
-}
boolExp :: Parser BoolExp
boolExp = chainl1 boolTerm orOp

boolTerm :: Parser BoolExp
boolTerm = chainl1 boolFactor andOp          

boolFactor :: Parser BoolExp
boolFactor = do {reservedOp lis "("; p<-boolExp; reservedOp lis ")"; return p}
            <|> do {reservedOp lis "~"; p<-boolFactor; return (Not p)}
            <|> do {reserved lis "true"; return (BTrue)}
            <|> do {reserved lis "false"; return (BFalse)} 
            <|> do p<-intExp
                   (do {reservedOp lis ">"; q<-intExp; return (Gt p q)}
                    <|> do {reservedOp lis "<"; q<-intExp; return (Lt p q)}
                    <|> do {reservedOp lis "="; q<-intExp; return (Eq p q)})

orOp :: Parser (BoolExp -> BoolExp -> BoolExp)
orOp = do {reservedOp lis "|"; return Or}

andOp :: Parser (BoolExp -> BoolExp -> BoolExp)
andOp = do {reservedOp lis "&"; return And}
          
--------------------------
--- Parser de comandos ---
--------------------------
{-
commExp  -> commTerm ';' commExp | commTerm
commTerm -> skip | 'if' boolExp 'then' commTerm 'else' commTerm 'end'
            | 'while' boolExp 'do' commTerm 'end' | <var> ':=' intExp
            | 'repeat' commTerm 'until' boolExp 'end'
-}

commExp :: Parser Comm
commExp = chainl1 commTerm commOp

commTerm :: Parser Comm
commTerm = do {reserved lis "skip";return Skip}
          <|> do {reserved lis "if"; p<-boolExp
                  ;reserved lis "then"; q<-commExp
                  ;reserved lis "else"; r<-commExp
                  ;reserved lis "end"; return (Cond p q r)}
          <|> do {reserved lis "while"; p<-boolExp
                  ;reserved lis "do"; q<-commExp
                  ;reserved lis "end"; return (While p q)}
          <|> do {p<-identifier lis; reservedOp lis ":=";
                  ;q<-intExp; return (Let p q)}
      
commOp :: Parser (Comm -> Comm -> Comm)
commOp = do {reservedOp lis ";"; return Seq}

-------------------------
--- Función de parseo ---
-------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser commExp)
