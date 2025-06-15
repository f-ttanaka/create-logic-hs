module IO.Parser (
  parseProp,
  parseProps,
) where

import Common
import Prop
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

reservedOps :: [String]
reservedOps =
  [ "->"
  , "|"
  , "&"
  , "~"
  ]

lexer :: Tok.GenTokenParser String () Identity
lexer =
  Tok.makeTokenParser $
    Tok.LanguageDef
      { Tok.commentStart = "{-"
      , Tok.commentEnd = "-}"
      , Tok.commentLine = "--"
      , Tok.nestedComments = True
      , Tok.identStart = upper <|> char '_'
      , Tok.identLetter = alphaNum <|> oneOf "_'"
      , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.reservedNames = []
      , Tok.reservedOpNames = reservedOps
      , Tok.caseSensitive = True
      }

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

type Op a = Ex.Operator String () Identity a

type Operators a = Ex.OperatorTable String () Identity a

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x op = Ex.Infix (reservedOp x >> return op)

infixOpAL :: String -> (a -> a -> a) -> Op a
infixOpAL x op = infixOp x op Ex.AssocLeft

contents :: Operators a -> Parser a -> Parser a
contents = Ex.buildExpressionParser

aprop :: Parser Prop
aprop = parens prop <|> (Var <$> identifier)

table :: Operators Prop
table =
  [
    [ infixOpAL "->" Imply
    ]
  ,
    [ infixOpAL "&" And
    , infixOpAL "|" Or
    ]
  ,
    [ Ex.Prefix (reservedOp "~" >> return Not)
    ]
  ]

prop :: Parser Prop
prop = contents table aprop

parseProp :: String -> Either ParseError Prop
parseProp input = parse prop "<stdin>" input

parseProps :: String -> Either ParseError [Prop]
parseProps input = parse (semiSep1 prop) "<stdin>" input
