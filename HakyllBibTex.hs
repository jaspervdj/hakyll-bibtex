--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HakyllBibTex
    ( BibEntry (..)
    , bibEntryContext
    , BibFile (..)
    , bibFileEntryNames
    , bibFileCompiler
    , parseBibFile
    , lookupBibEntry
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Binary         (Binary (..))
import           Data.Typeable       (Typeable)
import           Hakyll
import qualified Text.BibTeX.Entry   as BibTex
import qualified Text.BibTeX.Format  as BibTex.Format
import qualified Text.BibTeX.Parse   as BibTex.Parse
import qualified Text.Pandoc         as Pandoc
import qualified Text.Parsec         as Parsec


--------------------------------------------------------------------------------
newtype BibEntry = BibEntry BibTex.T
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary BibEntry where
    put (BibEntry t) = do
        put $ BibTex.entryType t
        put $ BibTex.identifier t
        put $ BibTex.fields t

    get = BibEntry <$> (BibTex.Cons <$> get <*> get <*> get)


--------------------------------------------------------------------------------
instance Writable BibEntry where
    write fp item =
        let BibEntry t = itemBody item
        in writeFile fp (BibTex.Format.entry t)


--------------------------------------------------------------------------------
bibEntryContext :: Context BibEntry
bibEntryContext = Context $ \key item ->
    let BibEntry t = itemBody item
    in case key of
        "identifier" -> return $ BibTex.identifier t
        _            -> case lookup key (BibTex.fields t) of
            Nothing  -> empty
            Just val -> return $ latexToHtml val
  where
    -- Renders latex to HTML, but don't wrap everything in a <p>...
    latexToHtml tex =
        let p = case Pandoc.readLaTeX Pandoc.defaultParserState tex of
                    Pandoc.Pandoc meta [Pandoc.Para para] ->
                        Pandoc.Pandoc meta [Pandoc.Plain para]
                    x                                     -> x
        in Pandoc.writeHtmlString Pandoc.defaultWriterOptions p


--------------------------------------------------------------------------------
newtype BibFile = BibFile [BibEntry]
    deriving (Binary, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable BibFile where
    write fp item =
        let BibFile entries = itemBody item
        in writeFile fp $ concat
            [BibTex.Format.entry entry | BibEntry entry <- entries]


--------------------------------------------------------------------------------
bibFileEntryNames :: BibFile -> [String]
bibFileEntryNames (BibFile es) = [BibTex.identifier t | BibEntry t <- es]


--------------------------------------------------------------------------------
bibFileCompiler :: Compiler (Item BibFile)
bibFileCompiler = fmap parseBibFile <$> getResourceString


--------------------------------------------------------------------------------
parseBibFile :: String -> BibFile
parseBibFile string = case Parsec.parse BibTex.Parse.file "<bib file>" string of
    Left err -> error $ show err
    Right xs -> BibFile $ map (BibEntry . BibTex.lowerCaseFieldNames) xs


--------------------------------------------------------------------------------
lookupBibEntry :: String -> BibFile -> BibEntry
lookupBibEntry name (BibFile es) =
    case [BibEntry t | BibEntry t <- es, BibTex.identifier t == name] of
        []      -> error $ name ++ " not found in BibFile"
        (x : _) -> x
