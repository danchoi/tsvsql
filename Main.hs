{-# LANGUAGE OverloadedStrings, ScopedTypeVariables#-}
module Main where
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad (when)
import Data.Attoparsec.Text 
import System.Environment (getArgs)
import qualified Options.Applicative as O

data Options = Options {  
      template :: Template
    } deriving Show

data Template = TemplateFile FilePath | TemplateText Text deriving (Show)

parseOpts :: O.Parser Options
parseOpts = Options <$> (tmplText <|> tmplFile)

tmplText = TemplateText . T.pack <$> O.argument O.str (O.metavar "TEMPLATE")
tmplFile = TemplateFile 
      <$> O.strOption (O.metavar "FILE" <> O.short 'f' <> O.help "Template file")

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
          <> O.progDesc "Inject TSV into SQL template strings" 
          <> O.header "tsvsql 0.1.1.0")

main = do
  Options tmpl <- O.execParser opts
  template <- case tmpl of
                  TemplateFile fp -> T.readFile fp
                  TemplateText t -> return t
  xs :: [[Text]] <- fmap (map (T.splitOn "\t") . T.lines) T.getContents 
  let chunks :: [TemplateChunk] 
      chunks = parseText template
      results = map (evalText chunks) xs
  mapM_ T.putStrLn results

data ValType = String | Bool | Number deriving (Eq, Show)

data TemplateChunk = Pass Text 
    | Placeholder Int ValType 
    deriving (Show, Eq)

evalText :: [TemplateChunk] -> [Text] -> Text
evalText xs vals = mconcat $ map (evalChunk vals) xs

evalChunk :: [Text] -> TemplateChunk -> Text
evalChunk vs (Pass s) = s
evalChunk vs (Placeholder idx _) | (vs !! idx) == "null" = "NULL"
evalChunk vs (Placeholder idx String) = wrapQuote (vs !! idx)
evalChunk vs (Placeholder idx Number) | (vs !! idx) == "" = "NULL"
                                      | otherwise         = (vs !! idx)
evalChunk vs (Placeholder idx Bool) | (vs !! idx) == "t" = "true"
evalChunk vs (Placeholder idx Bool) | (vs !! idx) == "f" = "false"

wrapQuote x = T.singleton '\'' <> (escapeText x) <> T.singleton '\''

escapeText = T.pack . escapeStringLiteral . T.unpack 

escapeStringLiteral :: String -> String
escapeStringLiteral ('\'':xs) = '\'': ('\'' : escapeStringLiteral xs)
escapeStringLiteral (x:xs) = x : escapeStringLiteral xs
escapeStringLiteral [] = []

parseText :: Text -> [TemplateChunk]
parseText = either error id . parseOnly (many textChunk)

textChunk = placeholderChunk <|> passChunk

placeholderChunk :: Parser TemplateChunk
placeholderChunk = do
    try (char '$')
    idx <- decimal
    type' <- pType
    return $ Placeholder (idx - 1) type'

pType :: Parser ValType
pType = 
  (do
    try (char ':') 
    (Bool <$ string "bool") <|> (Number <$ string "num"))
  <|> pure String
  
passChunk :: Parser TemplateChunk
passChunk = Pass <$> takeWhile1 (notInClass "$")


------------------------------------------------------------------------

