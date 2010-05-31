module Main
where
import Prelude
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad
import Text.StringTemplate
import Text.Pandoc
import Text.Pandoc.Shared
import Data.Bool.Higher
import System
import System.IO.Unsafe
import System.Locale
import System.Environment
import System.Directory
import System.FilePath 
import System.Process (readProcess, readProcessWithExitCode)

-- utility functions
(~>) = flip ($)
infixl 1 ~>

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = foldM (\ (l,r) e -> return . ((e:l,r) ?? (l,e:r)) =<< f e) ([],[]) $ reverse xs

a >>> b = (unsafePerformIO $ putStrLn $ show a) `seq` b
p a = (unsafePerformIO $ putStrLn $ take 150 $ show a) `seq` a

ordinalize :: (Integral a) => a -> String
ordinalize n 
    | elem n [11..13] = show n ++ "th"
    | n `mod` 10 == 1 = show n ++ "st"
    | n `mod` 10 == 2 = show n ++ "nd"
    | n `mod` 10 == 3 = show n ++ "rd"

-- TODO: format dates web2.0-style (e.g. "yesterday", "3 days ago") from javascript;
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale ("%e %B %Y")

formatDateAtom :: UTCTime -> String
formatDateAtom = formatTime defaultTimeLocale ("%FT%TZ")

-- data declarations
data Site = Site {
      sitePages :: [Page], 
      siteTemplates :: STGroup String
    }

data Page = Page {
      pageFile :: String, 
      pageTitle :: String,
      pageContent :: String, 
      pagePublished :: UTCTime
    }

-- teh program
isPageFile file = 
    ((== ".pandoc").takeExtension) file

commitDate :: String -> IO UTCTime
commitDate path = do
  oldcd <- getCurrentDirectory
  setCurrentDirectory $ takeDirectory path
  (exitCode, dateStr, stdErr) <- readProcessWithExitCode "git" ["log", "--format=%at", "-1", (takeFileName path)] ""
  setCurrentDirectory oldcd
  maybe getCurrentTime return (parseTime defaultTimeLocale "%s" dateStr)

pandocWriterOptions =  defaultWriterOptions { writerEmailObfuscation = ReferenceObfuscation }

loadPage :: String -> IO Page
loadPage path = do
  content <- readFile path
  title <- return $ loadMeta content 0
  published <- return $ loadMeta content 2
  fileName <- return (dropExtension $ takeFileName path)
  commitDate' <- commitDate path
  return $ Page 
         fileName 
         (fromMaybe fileName title)
         ((writeHtmlString pandocWriterOptions.readMarkdown defaultParserState) content)
         (fromMaybe commitDate' (parseTime defaultTimeLocale "%Y-%m-%d %H:%M" (fromMaybe ""  published)))
    where loadMeta content line = 
              let ls = lines content in
              if ((length ls) >= line && head (ls !! line) == '%')
              then Just (ls !! line ~> drop 1 ~> dropWhile (== ' '))
              else Nothing

buildPage :: StringTemplate String -> Page -> String
buildPage template page@(Page file title content published) =
    let
        attributes = [("content", [content]), 
                      ("title", [title]), 
                      ("published", [formatDate published])]
        templateAttr = setManyAttrib attributes template 
    in
       render templateAttr


getFilesR :: String -> IO [String]
getFilesR path = do
  e <- doesDirectoryExist path
  if e
       then getDirectoryContents path >>= filterM (return.not.(flip elem ['.', '_']).head)  >>= mapM (getFilesR.(path </>)) >>= ((return.join :: [[a]] -> IO [a]))
       else return [path]


getAttribs posts = [
 ("updated", [formatDateAtom $ pagePublished (last posts)]),
 ("updatedAtom", [formatDateAtom $ pagePublished (last posts)]),
 ("postTitles", (map pageTitle posts)),
 ("postContents", (map pageContent posts)),
 ("postDates", map (formatDate.pagePublished) posts),
 ("postDatesAtom", map (formatDateAtom.pagePublished) posts),
 ("postAddresses", map ((++ ".html").pageFile) posts)]

buildBlog :: STGroup String -> FilePath -> FilePath -> IO ()
buildBlog templates postsDir outDir = 
    let [postTemplate, blogTemplate, feedTemplate] = map (fromJust.flip getStringTemplate templates) ["post", "blog", "feed"] in
    do
      posts <- getDirectoryContents postsDir >>= 
               filterM (return.isPageFile) >>= 
               mapM (loadPage.(postsDir </>)) >>= 
               return.(sortBy (\ p1 p2 -> (pagePublished p1) `compare` (pagePublished p2)))
      createDirectoryIfMissing True outDir
      -- build each post
      mapM (\ p -> writeFile 
            (outDir </> (dropExtension (pageFile p)) ++ ".html")
            (buildPage postTemplate p)) posts

      -- build the index
      writeFile (outDir </> "index.html") (render $ setManyAttrib (getAttribs posts) blogTemplate)
      -- build the feed
      writeFile (outDir </> "feed.xml") (render $ setManyAttrib (getAttribs (reverse $ take 10 posts)) feedTemplate)
      return ()

buildSite :: String -> IO ()
buildSite dir = do
  templates <- directoryGroup (dir </> "_templates") :: IO (STGroup String)
  (pages, static) <- getFilesR dir >>= partitionM (return.isPageFile)
  outDir <- return (dir </> "_site")
  (doesDirectoryExist outDir >>= (removeDirectoryRecursive outDir) ?? (return ()))
  createDirectory outDir
  -- copy static files
  mapM (\ f ->
          let outFile = (outDir </> makeRelative dir f) in
          createDirectoryIfMissing True (takeDirectory outFile) >> copyFile f outFile) static
  -- generate regular pages
  indexTemplate <- return $ fromJust (getStringTemplate "index" templates)
  mapM (\ p -> do
          page <- loadPage p
          outFile <- return (outDir </> makeRelative dir (dropExtensions p) ++ ".html")
          createDirectoryIfMissing True (takeDirectory outFile)
          html <- return $ buildPage indexTemplate page
          writeFile outFile html) pages
  -- generate posts, post list and atom feed
  doesDirectoryExist (dir </> "_posts") >>= buildBlog templates (dir </> "_posts") (outDir </> "blog") ?? return ()

  return ()

