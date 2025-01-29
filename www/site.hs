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
main = hakyllWith (def {providerDirectory = ".."}) $ do
    match ("items/**.jpg" .||. "items/**.png" .||. "items/**.gif") $ do
        route   removeInitialComponent
        compile $ loadImage
    match "www/styles.css" $ do
        route   $ removeInitialComponent
        compile compressCssCompiler

    --- Items
    match itemPattern $ do
        route   $ composeRoutes removeInitialComponent $ setExtension "html"
        compile itemCompiler

    --- Listings
    match "index.md" $ do
        route   $ setExtension "html"
        compile $ listingCompiler
    match "periods/*.md" $ do
        route   $ setExtension "html"
        compile $ listingCompiler

    match "www/templates/*" $ compile templateBodyCompiler


itemPattern = "items/**.md"
itemCompiler =
    do id <- getUnderlying
       mdCompiler
         >>= saveSnapshot "content"
         >>= loadAndApplyTemplate "www/templates/item.html" defaultContext
         >>= loadAndApplyTemplate "www/templates/main.html" defaultContext
         >>= relativizeUrls

listingCompiler =
    do id <- getUnderlying
       mdCompiler
         >>= saveSnapshot "content"
         >>= loadAndApplyTemplate "www/templates/period.html" (periodsCtx id)
         >>= loadAndApplyTemplate "www/templates/main.html"   defaultContext
         >>= relativizeUrls

articleCtx = defaultContext

periodsCtx id =
                listField "periods"  (periodCtx id) (loadAllSnapshots "periods/*.md" "content") `mappend`
                listField "articles" articleCtx     (loadAllSnapshots itemPattern "content") `mappend`
                defaultContext
    where periodCtx id =
                field "current" (\i -> if id == itemIdentifier i then return "current"
                                                                 else fail "other") `mappend`
                (field "periodStart" (\i -> do return $ start $ itemIdentifier i)) `mappend`
                (field "periodEnd"   (\i -> do return $ end   $ itemIdentifier i)) `mappend` defaultContext
          startEnd id = take 2 $ splitAll "-" $ last $ splitDirectories $ dropExtension $ toFilePath id
          start id = head $ startEnd id
          end id = last $ startEnd id

mdCompiler = pandocCompilerWith readerOptions writerOptions
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
