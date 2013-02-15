{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>), mconcat)
import Hakyll

ctxt :: Context String
ctxt =
  modificationTimeField "date" "%B %e, %Y, %H:%M" <>
  defaultContext
    
main = hakyll $ do
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
        >>= loadAndApplyTemplate "Templates/default.html" ctxt      
        >>= readPandocBiblio defaultHakyllReaderOptions (Just csl) bib
        >>= return . writePandoc
