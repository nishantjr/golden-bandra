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

    match "src/**.md" $ do
        route   $ composeRoutes removeInitialComponent $
                                setExtension "html"
        compile $ do
            myPandocCompiler
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

--------------------------------------------------------------------------------

removeInitialComponent :: Routes
removeInitialComponent = customRoute $ tailFilePath . toFilePath
    where tailFilePath path = case (splitPath path) of
                                   p:ps -> joinPath ps
                                   []   -> error "empty path"
