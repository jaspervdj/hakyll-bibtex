--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HakyllBibTex
    ( BibEntry (..)
    , bibEntryCompiler
    , bibEntryContext
    , BibFile (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Binary         (Binary (..))
import           Data.Functor        ((<$))
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
bibEntryCompiler :: Compiler (Item BibEntry)
bibEntryCompiler = do
    item <- getResourceString
    let source = toFilePath $ itemIdentifier item
    case Parsec.parse BibTex.Parse.file source (itemBody item) of
        Left err  -> fail $ show err
        Right [x] -> return $ BibEntry (BibTex.lowerCaseFieldNames x) <$ item
        Right _   -> fail "Need exactly one bib entry per file"


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
