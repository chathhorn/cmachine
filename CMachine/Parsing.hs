-- Provides parsers for the CMa language as well as some additional commands
-- for controlling the CMachine state from, e.g., the command line interface.
--
module CMachine.Parsing 
      ( fileParser, cliParser
      , cmasList, cmdsList -- TODO remove
      , CMaParser
      , Cmd (..) 
      ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Combinator

import Control.Applicative ((<*), (<$>))
import Control.Monad
import Control.Monad.Trans
import Control.Arrow (second)

import CMachine
import CMachine.Core
import CMachine.Syntax

-- Parsers for CMa instruction of no arguments.
cma0 :: [(String, CMaParser ())]
cma0 = mapSnd (lift . pushi)
       [ ("add", Add)
       , ("and", And)
       , ("call", Call)
       , ("div", Div)
       , ("dup", Dup)
       , ("eq", Eq)
       , ("geq", Geq)
       , ("gr", Gr)
       , ("halt", Halt)
       , ("leq", Leq)
       , ("le", Le)
       , ("load", Load)
       , ("mark", Mark)
       , ("mod", Mod)
       , ("mul", Mul)
       , ("neg", Neg)
       , ("neq", Neq)
       , ("new", New)
       , ("nop", Nop)
       , ("not", Not)
       , ("or", Or)
       , ("pop", Pop)
       , ("store", Store)
       , ("sub", Sub)
       ]

-- Parsers for CMa instructions of 1 integer argument.
cma1 :: [(String, CMaParser ())]
cma1 = mapSnd (\c -> do
      i <- pInt 
      lift $ pushi (c i))
      [ ("alloc", Alloc)
      , ("enter", Enter)
      , ("jump", Jump)
      , ("jumpz", JumpZ)
      , ("jumpi", JumpI)
      , ("loada", LoadA)
      , ("loadc", LoadC)
      , ("load", LoadN)
      , ("loadr", LoadR)
      , ("loadrc", LoadRC)
      , ("return", Return)
      , ("store", StoreN)
      , ("storea", StoreA)
      , ("storer", StoreR)
      ]

-- Parsers for CMa instructions of 2 integer arguments.
cma2 :: [(String, CMaParser ())]
cma2 = mapSnd (\c -> do 
      i <- pInt
      j <- pInt 
      lift $ pushi (c i j))
      [ ("loadr", LoadRN)
      , ("slide", Slide)
      , ("storer", StoreRN)
      ]

-- Parsers for extra commands.
cmd :: [(String, CMaParser Cmd)]
cmd = [ (":step", lift step >> return Step)
      , (":execute", return Exec)
      , (":clear", (
            pReserved "code" >>
            return ClearCode) 
      <|> (pReserved "data" >>
            return ClearData)
      <|> (pReserved "watches" >>
            return ClearWatches)
      <|> return Clear
      )
      , (":toggle", (
            pReserved "stack" >>
            return ToggleStack) 
      <|> (pReserved "data" >>
            return ToggleData)
      <|> (pReserved "code" >>
            return ToggleCode)
      )
      , (":watch", do 
            n <- pInt 
            i <- pInt 
            return $ Watch n i
      )
      , (":quit", return Quit)
      , (":help", return Help)
      , (":pushi", cmaParser)
      , (":resize", ResizeData <$> pInt)
      , (":load", LoadFile <$> pFilePath)
      ]

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map $ second f

data Cmd = Quit 
         | LoadFile String | LoadFileResult [Cmd]
         | ParseFailed 
         | InsLoad | Step | LoadAndStep
         | Exec | Clear | ClearCode | ClearData | ClearWatches | ResizeData Int | Help
         | ToggleStack | ToggleData | ToggleCode
         | Watch Int Int 
         deriving (Show)

def :: GenLanguageDef String () (CMachine CMa)
def = LanguageDef
      { commentStart   = ""
      , commentEnd     = ""
      , commentLine    = ""
      , nestedComments = True
      , identStart     = letter <|> char '_'
      , identLetter    = alphaNum <|> oneOf "_'"
      , opStart        = opLetter def
      , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , reservedOpNames= []
      , reservedNames  = cmasList ++ cmdsList ++ ["code", "data"]
      , caseSensitive  = True
      }

lexer = makeTokenParser def

pReserved :: String -> CMaParser ()
pReserved = reserved lexer

pInt :: CMaParser Int
pInt = fromInteger <$> integer lexer

pWhiteSpace :: CMaParser ()
pWhiteSpace = whiteSpace lexer

pFilePath :: CMaParser String
pFilePath = many1 (noneOf ":*+?<>")

pEol :: CMaParser ()
pEol = void (char '\n' <|> (char '\r' >> option '\n' (char '\n')))

type CMaParser c = ParsecT String () (CMachine CMa) c

makeParsers = map $ \(t, c) -> try ((pReserved t >> c) <* optional pEol)

foldParsers = foldr (<|>) mzero

andStep = map (>> (lift step >> return LoadAndStep))

cmaParsers = map (>> return InsLoad) $
      makeParsers cma2 ++ 
      makeParsers cma1 ++ 
      makeParsers cma0

cmdParsers = makeParsers cmd

cmaParser = pWhiteSpace >> foldParsers cmaParsers

cmasList, cmdsList :: [String]
cmasList = map fst cma0 ++ map fst cma1 ++ map fst cma2 
cmdsList = map fst cmd

fileParser :: CMaParser [Cmd]
fileParser = pWhiteSpace >> many1 (foldParsers cmaParsers)

cliParser :: CMaParser [Cmd]
cliParser = pWhiteSpace >> many1 (foldParsers $ andStep cmaParsers ++ cmdParsers)

