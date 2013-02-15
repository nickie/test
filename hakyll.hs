{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>), mconcat)
import System.Locale (defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import Hakyll

main = do
  t <- getZonedTime
  let time = formatTime defaultTimeLocale "%B %e, %Y, %H:%M %Z" t
  let ctxt = constField "date" time <>
             defaultContext
  hakyll $ do
    -- Copy the hook
    match "hook.php" $ do
      route   idRoute
      compile copyFileCompiler
    -- Read templates
    match "Templates/*" $ do
      compile templateCompiler
    -- Compress CSS
    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler
    -- Render bibliography
    match "*.csl" $ do
      compile cslCompiler
    match "bib/**.bib" $ do
      compile biblioCompiler
    -- Render publication list
    match "index.md" $ do
      route $ setExtension "html"
      compile $ do
        csl <- load "elsevier-harvard.csl"
        bibs <- loadAll "bib/**.bib"
        let bib = Item "references.bib" . Biblio $ 
                  concatMap ((\(Biblio refs) -> refs) . itemBody) bibs
        pandocCompiler
          >>= readPandocBiblio defaultHakyllReaderOptions (Just csl) bib
          >>= return . writePandoc
          >>= loadAndApplyTemplate "Templates/default.html" ctxt      
