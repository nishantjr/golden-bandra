--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import          Data.Monoid (mappend)
import          System.FilePath
import          Text.Pandoc.Options
import          Text.Pandoc.Extensions
import          Hakyll
import          Hakyll.Images   (loadImage, ensureFitCompiler)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "src/**.jpg" $ do
        route   removeInitialComponent
        compile $ loadImage

    match "src/styles.css" $ do
        route   $ removeInitialComponent
        compile compressCssCompiler

    match "src/**.md" $ do
        route   $ composeRoutes removeInitialComponent $
                                setExtension "html"
        compile $ do
            pandocCompilerWith pandocReaderOptions (def::WriterOptions)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

  where
     pandocReaderOptions = def { readerExtensions = extensionsFromList [Ext_line_blocks, Ext_fenced_divs] }

--------------------------------------------------------------------------------

removeInitialComponent :: Routes
removeInitialComponent = customRoute $ tailFilePath . toFilePath
    where tailFilePath path = case (splitPath path) of
                                   p:ps -> joinPath ps
                                   []   -> error "empty path"
