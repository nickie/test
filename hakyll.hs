{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Function       (on)
import           Data.List           (groupBy, isSuffixOf, sortBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mconcat, (<>))
import           Data.Ord            (comparing)
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
        match "pub/*.md" $ do
            route $ setExtension "html"
            compile pandocCompiler

        -- Render publication list
        match "index.md" $ do
            route $ setExtension "html"
            compile $ do
                pubs <- loadAll "pub/*.md"
                meta <- mapM (getMetadata . itemIdentifier) pubs
                let pubsWithMeta = zip pubs meta
                    yearedPubs = [ (year, p)
                                 | (year, _, p) <- getPubsByType pubsWithMeta "conference"]
                    -- Group items by "year"; returns [ (Year, [Content]) ]:
                    confPubs = [ (year, ps)
                               | p <- (groupBy ((==) `on` fst) . sortBy (comparing fst)) yearedPubs
                               , let (year, ps) = (fst $ head p, map snd p)]
                --debugCompiler $ "----- Conference publications are = " ++ show confPubs
                ps   <- mapM (pubList "year") confPubs
                let allPubs = mconcat $ map itemBody ps
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/index.html"
                        (constField "publications" allPubs <> defaultContext)
                    >>= loadAndApplyTemplate "templates/default.html"
                        (constField "date" time <> defaultContext)
                    >>= relativizeUrls
                    >>= cleanIndexUrls

-- | Filters publications based on `publication` type (metadata).
getPubsByType :: [(Item String, Metadata)]
              -> String                          -- publication type
              -> [(String, String, Item String)] -- (Year, Month, Content)
getPubsByType ps pType =
    [ (pubYear, pubMonth, i) | (i, m) <- ps
    , let pubYear  = fromMetaWithDefault "Unknown year" "year" m
          pubType  = fromMetaWithDefault "dissemination" "publication" m
          pubMonth = fromMetaWithDefault "Unknown month" "month" m
    , pubType == pType
    ]

-- | Looks in metadata `m` for `key` and returns the appropriate value
-- or `def` (if not found).
fromMetaWithDefault :: String -> String -> Metadata -> String
fromMetaWithDefault def key m = fromMaybe def (M.lookup key m)

-- | Creates a pub-list.html for the publications.
pubList :: String                  -- const field (in template)
        -> (String, [Item String]) -- (field value, [Content])
        -> Compiler (Item String)
pubList f (fVal, pubs) = do
    pubTmpl <- loadBody "templates/pub-item.html"
    list    <- applyTemplateList pubTmpl defaultContext pubs
    l       <- makeItem list -- Extract an `Item a` to use below
    loadAndApplyTemplate "templates/pub-list.html"
        (constField f fVal <> constField "publications" list <> defaultContext) l

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanAll)
  where cleanAll url = foldl clean url ["index.html", "index.php"]
        clean url idx
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url
