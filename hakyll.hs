{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List           (isSuffixOf)
import           Data.Monoid         ((<>))
import           Data.Time.Format    (formatTime)
import           Data.Time.LocalTime (getZonedTime)
import           System.Locale       (defaultTimeLocale)

import           Hakyll

main :: IO ()
main = do
    t <- getZonedTime
    let time = formatTime defaultTimeLocale "%B %e, %Y, %H:%M %Z" t

    hakyll $ do
        -- Copy static things
        match ("hook.php" .||. "images/*") $ do
            route   idRoute
            compile copyFileCompiler

        -- Read templates
        match "templates/*" $ compile templateCompiler

        -- Compress CSS
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        -- Render publications
        match "pub/*/*.md" $ do
            route $ setExtension "html"
            compile pandocCompiler

        -- Render publication list
        match "index.md" $ do
            route $ setExtension "html"
            compile $ do
                pubs12 <- genPubsPerYear "2012"
                pubs13 <- genPubsPerYear "2013"
                let allPubs = itemBody pubs12 <> itemBody pubs13
                pandocCompiler
                    -- Merge all <ul>s in index.html
                    >>= loadAndApplyTemplate "templates/index.html"
                        (constField "publications" allPubs <> defaultContext)
                    >>= loadAndApplyTemplate "templates/default.html"
                        (constField "date" time <> defaultContext)
                    >>= relativizeUrls
                    >>= cleanIndexUrls

-- | Creates a <ul> list of publications (an `Item String`) in `year`.
genPubsPerYear :: String -> Compiler (Item String)
genPubsPerYear year = do
    pubs    <- loadAll $ fromGlob ("pub/" ++ year ++ "/*.md")
    -- Create <li> items
    pubTmpl <- loadBody "templates/pub-item.html"
    list    <- applyTemplateList pubTmpl defaultContext pubs
    -- Create <ul> per year
    l       <- makeItem list -- Extract an `Item a` to use below
    loadAndApplyTemplate "templates/pub-list.html"
        (constField "year" year <> constField "publications" list <> defaultContext) l

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanAll)
  where cleanAll url = foldl clean url ["index.html", "index.php"]
        clean url idx
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url
