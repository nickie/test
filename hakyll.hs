{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad       ((>=>))
import Data.Monoid         ((<>))
import Data.List           (isSuffixOf)
import Data.Time.Format    (formatTime)
import Data.Time.LocalTime (getZonedTime)
import System.Locale       (defaultTimeLocale)

import Hakyll

main :: IO ()
main = do
  t <- getZonedTime
  let time = formatTime defaultTimeLocale "%B %e, %Y, %H:%M %Z" t
      ctxt = constField "date" time <> defaultContext
      finish = loadAndApplyTemplate "Templates/default.html" ctxt
        >=> relativizeUrls
        >=> cleanIndexUrls

  hakyll $ do
    -- Copy the hook
    match "hook.php" $ do
      route   idRoute
      compile copyFileCompiler

    -- Read templates
    match "Templates/*" $ compile templateCompiler

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
        pubs    <- loadAll "pub/*/*.md"
        pubTmpl <- loadBody "Templates/pub-item.html"
        list    <- applyTemplateList pubTmpl ctxt pubs
        pandocCompiler
          >>= loadAndApplyTemplate "Templates/index.html"
                (constField "publications" list <> defaultContext)
          >>= finish

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanAll)
  where cleanAll url = foldl clean url ["index.html", "index.php"]
        clean url idx
          | idx `isSuffixOf` url = take (length url - length idx) url
          | otherwise            = url
