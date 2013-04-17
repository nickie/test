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
                    confPubs     = getPubsByType pubsWithMeta "conference"
                -- debugCompiler $ "----- Conference publications are = " ++ show confPubs
                ps   <- mapM pubList confPubs
                let allPubs = mconcat $ map itemBody ps
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/index.html"
                        (constField "publications" allPubs <> defaultContext)
                    >>= loadAndApplyTemplate "templates/default.html"
                        (constField "date" time <> defaultContext)
                    >>= relativizeUrls
                    >>= cleanIndexUrls

-- | Filters publications based on `publication` type (metadata).
-- Returns the list of publications grouped by `year` (i.e. (Year,
-- [Item String])).
getPubsByType :: [(Item String, Metadata)] -> String -> [(String, [Item String])]
getPubsByType ps pType = [ (year, is)
                         | p <- pubsByYear
                         , let (year, is) = (fst $ head p, map snd p)
                         ]
  where pubsOfType = [ (pubYear, i)
                     | (i, m) <- ps
                     , let pubYear = fromMetaWithDefault "Unknown year" "year" m
                           pubType = fromMetaWithDefault "dissemination" "publication" m
                     , pubType == pType
                     ]
        pubsByYear = (groupBy ((==) `on` fst) . sortBy (comparing fst)) pubsOfType

-- | Looks in metadata `m` for `key` and returns the appropriate value
-- or `def` (if not found).
fromMetaWithDefault :: String -> String -> Metadata -> String
fromMetaWithDefault def key m = fromMaybe def (M.lookup key m)

-- | Creates a pub-list.html for the publications (`pubs`) that
-- happened in `year`.
pubList :: (String, [Item String]) -> Compiler (Item String)
pubList (year, pubs) = do
    pubTmpl <- loadBody "templates/pub-item.html"
    list    <- applyTemplateList pubTmpl defaultContext pubs
    l       <- makeItem list -- Extract an `Item a` to use below
    loadAndApplyTemplate "templates/pub-list.html"
        (constField "year" year <> constField "publications" list <> defaultContext) l

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanAll)
  where cleanAll url = foldl clean url ["index.html", "index.php"]
        clean url idx
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url
