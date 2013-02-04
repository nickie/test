{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>), mconcat)
import Hakyll

main = hakyll $ do
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
