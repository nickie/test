{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Char           (toLower, toUpper)
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
                    yPubs = [ (year, p)
                            | (year, _, p) <- getPubsByType pubsWithMeta "conference"]
                    -- Group items by "year"; returns [ (Year, [Content]) ]:
                    confPubs = groupOnFst yPubs
                --debugCompiler $ "----- Conference publications are:\n" ++ show confPubs
                ps <- mapM (pubList "header") confPubs
                let allPubs = mconcat $ map itemBody ps
                datedCompiler time allPubs

        match "dissemination.md" $ do
            route $ setExtension "html"
            compile $ do
                pubs <- loadAll "pub/*.md"
                meta <- mapM (getMetadata . itemIdentifier) pubs
                let pubsWithMeta = zip pubs meta
                    ymPubs = [ (y, (m, p))
                             | (y, m, p) <- getPubsByType pubsWithMeta "dissemination"]
                    -- Group items first by "year" then by "month".
                    -- Returns [ (Year, [ (Month, [Content]) ]) ]:
                    dissPubs = [ (year, groupOnFst ms)
                               | (year, ms) <- groupOnFst ymPubs]
                    (years, monthedPubs) = unzip dissPubs
                allPubs <- yearedPubList years monthedPubs []
                datedCompiler time (itemBody allPubs)

-- | The compiler used for the Publication and the Dissemination web pages.
datedCompiler :: String                 -- ^ time of latest modification
              -> String                 -- ^ web-page content
              -> Compiler (Item String)
datedCompiler time content =
    pandocCompiler
        >>= loadAndApplyTemplate "templates/index.html"
            (constField "publications" content <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
            (constField "date" time <> defaultContext)
        >>= relativizeUrls
        >>= cleanIndexUrls

-- | Filters publications based on `publication` type (metadata).
getPubsByType :: [(Item String, Metadata)]
              -> String                          -- ^ publication type
              -> [(String, String, Item String)] -- ^ (Year, Month, Content)
getPubsByType ps pType =
    [ (pubYear, pubMonth, i) | (i, m) <- ps
    , let pubYear  = fromMetaWithDefault "Unknown year" "year" m
          pubType  = fromMetaWithDefault "dissemination" "publication" m
          pubMonth = fmt $ fromMetaWithDefault "Unknown month" "month" m
    , pubType == pType
    ]
  where fmt []       = []
        fmt (c : cs) = toUpper c : map toLower cs

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

-- | Creates a year-list.html for the Dissemination page using `pubList`.
yearedPubList :: [String]                    -- ^ years
              -> [[(String, [Item String])]] -- ^ [(Month, [Content])]
              -> String                      -- ^ accumulator
              -> Compiler (Item String)
yearedPubList [] _ acc = makeItem acc
yearedPubList _ [] _   = undefined -- should never match because:
                                   -- length ys == length ms
yearedPubList (y : ys) (m : ms) acc = do
    monthedPubs <- mapM (pubList "header") m
    let ps = mconcat $ map itemBody monthedPubs
    ps' <- makeItem ps
    yearedPubs <- mapM (compileWithYears ps) [ps']
    yearedPubList ys ms (acc ++ concatMap itemBody yearedPubs)
  where compileWithYears pubs =
            loadAndApplyTemplate "templates/year-list.html"
            (constField "header" y <> constField "publications" pubs <> defaultContext)

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanAll)
  where cleanAll url = foldl clean url ["index.html", "index.php"]
        clean url idx
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

-- | Groups a list of tuples based on the first item.
groupOnFst :: Ord a => [(a, b)] -> [(a, [b])]
groupOnFst l =
    [ (key, vs)
    | l' <- (groupBy ((==) `on` fst) . sortBy descending) l
    ,  let (key, vs) = (fst $ head l', map snd l')]
  where descending = flip $ comparing fst
