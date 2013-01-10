--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Hakyll
import           HakyllBibTex


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Preprocessing: get paper names
    names <- preprocess $ do
        bibFile <- parseBibFile <$> readFile "papers.bib"
        return $ bibFileEntryNames bibFile

    -- Compile bib database
    match "papers.bib" $ do
        route   idRoute
        compile bibFileCompiler

    -- Create a page for every known paper
    forM_ names $ \name ->
        create [fromCapture "papers/*.html" name] $ do
            route idRoute
            compile $ do
                -- Load bib database and extract the right entry
                bibFile <- loadBody "papers.bib"
                let bibEntry = lookupBibEntry name bibFile
                makeItem bibEntry
                    >>= loadAndApplyTemplate "paper.html" bibEntryContext

    -- Simple template
    match "paper.html" $
        compile templateCompiler
