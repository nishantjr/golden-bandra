--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
import Debug.Trace

import          Control.Monad       (filterM)
import          Control.Monad.ListM (sortByM)
import          Data.Maybe          (fromMaybe, maybeToList)
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
    tags <- buildTags itemPattern (fromCapture "tag:*")
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
        compile $ do
            mdCompiler
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "www/templates/item.html" itemCtx
              >>= applyMainTemplate

    --- Listings
    match "index.md" $ do
        route   $ setExtension "html"
        compile $ do getResourceBody
             >>= applyAsTemplate frontpageCtx
             >>= renderPandoc
             >>= listingCompiler []

    match (periodPattern .||. themePattern) $ do
        route   $ setExtension "html"
        compile $
            do tagName  <- takeFileName . dropExtension . toFilePath <$> getUnderlying
               articles <- itemsMatchingTag tags tagName
               mdCompiler
                 >>= saveSnapshot "content"
                 >>= listingCompiler articles

    match "www/templates/*" $ compile templateBodyCompiler


itemsMatchingTag :: Tags -> String -> Compiler [Item String]
itemsMatchingTag tags tag =
    loadAllSnapshots (fromList (fromMaybe [] $ lookup tag (tagsMap tags)))
                     "content"

--- Frontpage
frontpageCtx = listField "periods"  defaultContext (loadAllSnapshots periodPattern "content" >>= byPeriodStart) `mappend`
               listField "themes"   defaultContext (loadAllSnapshots themePattern  "content")                   `mappend`
               defaultContext

--- Items
itemPattern :: Pattern
itemPattern = "items/**.md"

itemCtx :: Context String
itemCtx = listField "tagt" defaultContext loadTagSnapshots `mappend` defaultContext
    where
      loadTagSnapshots =
          do ident <- getUnderlying
             mTags <- getMetadataField ident "tags"
             title <- getMetadataField ident "title"
             snapshots <- loadAllSnapshots (periodPattern .||. themePattern) "content"
             traceShow (length snapshots) (pure ())
             traceShow ((ident, mTags, title)) (pure ())
             return $ filter (\x -> matches (foldr (.||.) nothing (map (\t -> fromGlob $ "*/" ++ t ++ ".md")  $ fromMaybe [] $ fmap (splitAll ",") mTags))  (itemIdentifier x)) snapshots
      nothing = fromGlob "x" .&&. fromGlob "y" --- We want `complement everything`, but it is not exported.


--- Periods

periodPattern :: Pattern
periodPattern = "periods/*.md"

byPeriodStart :: MonadMetadata m => [Item a] -> m [Item a]
byPeriodStart = sortByM (comparingM periodStart)

periodStart :: MonadMetadata m => Item a -> m Int
periodStart i = do
    psStr <- getMetadataField (itemIdentifier i) "periodStart"
    return (fromMaybe 0 $ psStr >>= readMaybe)

periodCtx :: Context String
periodCtx = (field "periodStart" (\i -> do return $ start $ itemIdentifier i)) `mappend`
            (field "periodEnd"   (\i -> do return $ end   $ itemIdentifier i)) `mappend` defaultContext
    where startEnd id = take 2 $ splitAll "-" $ last $ splitDirectories $ dropExtension $ toFilePath id
          start id = head $ startEnd id
          end id = last $ startEnd id

themePattern :: Pattern
themePattern = "themes/*.md"

--- Listings

listingCompiler :: [Item String] -> Item String -> Compiler (Item String)
listingCompiler articles listingBody =
    do  pure listingBody
        >>= loadAndApplyTemplate "www/templates/period.html" (listingCtx (pure articles))
        >>= applyMainTemplate

listingCtx :: Compiler [Item String] -> Context String
listingCtx articles = listField "articles" itemCtx articles `mappend` defaultContext

--- Apply the main template and other nicities needed for a complete page.
applyMainTemplate :: Item String -> Compiler (Item String)
applyMainTemplate i = (loadAndApplyTemplate "www/templates/main.html" defaultContext) i >>= relativizeUrls

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

identifierReplaceExtension :: String -> Identifier -> Identifier
identifierReplaceExtension ext id = fromFilePath $ ((flip replaceExtension) ext) . toFilePath $ id

--- Utilities

comparingM :: (Monad m, Ord a) => (b -> m a) -> b -> b -> m Ordering
comparingM f x y = do
    xk <- (f x)
    yk <- (f y)
    return $ compare xk yk
