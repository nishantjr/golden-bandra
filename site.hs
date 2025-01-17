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
    match "src/CNAME" $ do
        route   removeInitialComponent
        compile $ copyFileCompiler

    match "src/**.jpg" $ do
        route   removeInitialComponent
        compile $ loadImage

    match "src/**.css" $ do
        route   $ removeInitialComponent
        compile compressCssCompiler

    match ( "src/index.md"
       .||. "src/gb/**.md"
       .||. "src/st-andrew-book/**.md"
       .||. "src/dlp-letter/**.md"
       )
        $ do
        route   $ composeRoutes removeInitialComponent $
                                setExtension "html"
        compile $ do
            myPandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "src/timeline/*.md"
        $ timeline


    match "templates/*" $ compile templateBodyCompiler

timeline = do
        route   $ composeRoutes removeInitialComponent $ setExtension "html"
        compile $ do
            id <- getUnderlying
            myPandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/timeline.html" (timelineCtx id)
                >>= relativizeUrls
    where timelineCtx id =
                listField "periods" (periodCtx id) (loadAllSnapshots "src/timeline/*.md" "content")
                `mappend` defaultContext
          periodCtx id =
                field "current" (\i -> if id == itemIdentifier i then return "current"
                                                                 else fail "other") `mappend`
                (field "periodStart" (\i -> do return $ start $ itemIdentifier i)) `mappend`
                (field "periodEnd"   (\i -> do return $ end   $ itemIdentifier i)) `mappend` defaultContext
          startEnd id = take 2 $ splitAll "-" $ last $ splitDirectories $ dropExtension $ toFilePath id
          start id = head $ startEnd id
          end id = last $ startEnd id


myPandocCompiler = pandocCompilerWithTransform readerOptions writerOptions usingSideNotes
  where readerOptions = def { readerExtensions = extensionsFromList readerExtensions }
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
