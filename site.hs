--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------------------------------------
import Hakyll
import HakyllBibTex


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "bib/*" $ do
        route $ setExtension "html"
        compile $ bibEntryCompiler
            >>= saveSnapshot "entry"
            >>= loadAndApplyTemplate "paper.html" bibEntryContext

    create ["papers.bib"] $ do
        route idRoute
        compile $ do
            entries <- loadAllSnapshots "bib/*" "entry"
            makeItem $ BibFile $ map itemBody entries

    match "paper.html" $
        compile templateCompiler
