--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import          Control.Monad.ListM (sortByM)
import          Data.Maybe          (fromMaybe)
import          Data.Monoid         (mappend)
import          Data.Ord
import          Hakyll
import          Hakyll.Images       (loadImage, ensureFitCompiler)
import          System.FilePath
import          Text.Pandoc.Extensions
import          Text.Pandoc.Options
import          Text.Pandoc.SideNote
import          Text.Read           (readMaybe)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith (def {providerDirectory = ".."}) $ do
    match "CNAME" $ do
        route   idRoute
        compile $ copyFileCompiler
    match ("items/**.jpg" .||. "items/**.png" .||. "items/**.gif") $ do
        route   removeInitialComponent
        compile $ loadImage
    match "items/**.svg" $ do
        route   removeInitialComponent
        compile $ copyFileCompiler

    match "www/styles.css" $ do
        route   $ removeInitialComponent
        compile $ copyFileCompiler

    --- Items
    match itemPattern $ do
        route   $ composeRoutes removeInitialComponent $ setExtension "html"
        compile itemCompiler

    --- Listings
    match "index.md" $ do
        route   $ setExtension "html"
        compile $ do getResourceBody
             >>= applyAsTemplate frontpageCtx
             >>= renderPandoc
             >>= listingCompiler

    match periodPattern $ do
        route   $ setExtension "html"
        compile $ do mdCompiler >>= saveSnapshot "content" >>= listingCompiler

    match "www/templates/*" $ compile templateBodyCompiler


--- Frontpage

frontpageCtx = listField "periods"  defaultContext (loadAllSnapshots "periods/*.md" "content" >>= byPeriodStart)
               `mappend` defaultContext


--- Items

itemPattern :: Pattern
itemPattern = "items/**.md"

itemCtx :: Context String
itemCtx = defaultContext

itemCompiler :: Compiler (Item String)
itemCompiler =
    do mdCompiler
         >>= saveSnapshot "content"
         >>= loadAndApplyTemplate "www/templates/item.html" itemCtx
         >>= applyMainTemplate


--- Periods

periodPattern :: Pattern
periodPattern = "periods/*.md"

byPeriodStart :: MonadMetadata m => [Item a] -> m [Item a]
byPeriodStart = sortByM (comparingM periodStart)

periodStart :: MonadMetadata m => Item a -> m Int
periodStart i = do
    psStr <- getMetadataField (itemIdentifier i) "periodStart"
    return (fromMaybe 0 $ psStr >>= readMaybe)

comparingM :: (Monad m, Ord a) => (b -> m a) -> b -> b -> m Ordering
comparingM f x y = do
    xk <- (f x)
    yk <- (f y)
    return $ compare xk yk

--- Listings

listingCompiler :: Item String -> Compiler (Item String)
listingCompiler item =
    do ident <- getUnderlying
       pure item
        >>= loadAndApplyTemplate "www/templates/period.html" (listingCtx ident)
        >>= applyMainTemplate
  where
    listingCtx :: Identifier -> Context String
    listingCtx id = listField "articles" itemCtx (loadAllSnapshots itemPattern    "content") `mappend`
                    defaultContext

--- Apply the main template and other nicities needed for a complete page.
applyMainTemplate :: Item String -> Compiler (Item String)
applyMainTemplate =     \i -> (loadAndApplyTemplate "www/templates/main.html" defaultContext) i
                    >>= relativizeUrls

renderMd :: Item String -> Compiler (Item String)
renderMd = renderPandocWith readerOptions writerOptions
  where readerOptions = def { readerExtensions = extensionsFromList readerExtensions }
        readerExtensions = [
               Ext_fancy_lists, Ext_fenced_divs, Ext_footnotes, Ext_implicit_figures,
               Ext_line_blocks, Ext_link_attributes, Ext_simple_tables, Ext_startnum,
               Ext_smart
           ]
        writerOptions = def::WriterOptions

mdCompiler :: Compiler (Item String)
mdCompiler = getResourceBody >>= renderMd

--- Routing

removeInitialComponent :: Routes
removeInitialComponent = customRoute $ tailFilePath . toFilePath
    where tailFilePath path = case (splitPath path) of
                                   p:ps -> joinPath ps
                                   []   -> error "empty path"
