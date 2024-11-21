--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import          Data.Monoid (mappend)
import          System.FilePath
import          Text.Pandoc.Options
import          Text.Pandoc.Extensions
import          Text.Pandoc.SideNote
import          Hakyll
import          Hakyll.Images   (loadImage, ensureFitCompiler)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "src/**.jpg" $ do
        route   removeInitialComponent
        compile $ loadImage

    match "src/*.css" $ do
        route   $ removeInitialComponent
        compile compressCssCompiler

    match "src/index.md" $ do
        route   $ composeRoutes removeInitialComponent $
                                setExtension "html"
        compile $ do getResourceBody
                     >>= applyAsTemplate articlesCtx
                     >>= renderPandoc
                     >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match (    "src/articles/*.md"
          .||. "src/dlp-letter/*.md"
          .||. "src/the-varsity-circle/*/*.md"
          )
      $ do
        route   $ composeRoutes removeInitialComponent $
                                setExtension "html"
        compile $ do
            myPandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

  where
     myPandocCompiler = pandocCompilerWithTransform readerOptions writerOptions usingSideNotes
     readerOptions = def { readerExtensions = extensionsFromList readerExtensions }
     readerExtensions = [
            Ext_fancy_lists,
            Ext_fenced_divs,
            Ext_footnotes,
            Ext_implicit_figures,
            Ext_line_blocks,
            Ext_link_attributes,
            Ext_simple_tables,
            Ext_startnum,
            Ext_smart
        ]
     writerOptions = def::WriterOptions

articlesCtx :: Context String
articlesCtx = listField "articles" defaultContext (loadAllSnapshots "src/*/**.md" "content")

--------------------------------------------------------------------------------

removeInitialComponent :: Routes
removeInitialComponent = customRoute $ tailFilePath . toFilePath
    where tailFilePath path = case (splitPath path) of
                                   p:ps -> joinPath ps
                                   []   -> error "empty path"

