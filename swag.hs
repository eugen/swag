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

-- utilities
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = foldM (\ (l,r) e -> return . ((e:l,r) ?? (l,e:r)) =<< f e) ([],[]) $ reverse xs

-- format a date for displaying it on a page
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale ("%e %B %Y")

-- format a date for inserting into an Atom feed
formatDateAtom :: UTCTime -> String
formatDateAtom = formatTime defaultTimeLocale ("%FT%TZ")

-- data declarations
data Page = Page {
      pageFile :: String, 
      pageTitle :: String,
      pageContent :: String, 
      pagePublished :: UTCTime
    }

-- returns true if a file should be processed by Swag, 
--         false if it should be directly copied to the output dir
isPageFile file = 
    ((== ".pandoc").takeExtension) file

-- gets the git commit date for a file
getCommitDate :: String -> IO UTCTime
getCommitDate path = do
  oldcd <- getCurrentDirectory
  setCurrentDirectory $ takeDirectory path
  (exitCode, dateStr, stdErr) <- readProcessWithExitCode "git" ["log", "--format=%at", "-1", (takeFileName path)] ""
  setCurrentDirectory oldcd
  maybe getCurrentTime return (parseTime defaultTimeLocale "%s" dateStr)

-- writer options used by swag
pandocWriterOptions =  defaultWriterOptions { writerEmailObfuscation = ReferenceObfuscation }

loadPage :: String -> IO Page
loadPage path = do
  content <- readFile path
  title <- return $ loadMeta content 0
  published <- return $ loadMeta content 2
  fileName <- return (dropExtension $ takeFileName path)
  commitDate <- getCommitDate path
  return $ Page 
         fileName 
         (fromMaybe fileName title)
         ((writeHtmlString pandocWriterOptions.readMarkdown defaultParserState) content)
         (fromMaybe commitDate (parseTime defaultTimeLocale "%Y-%m-%d %H:%M" (fromMaybe "" published)))
    where loadMeta content line = -- TODO: support multiline metadatata; also make it more robust 
              let ls = lines content in
              if ((length ls) >= line && head (ls !! line) == '%')
              then Just (dropWhile (== ' ') $ drop 1 $ ls !! line)
              else Nothing

buildPage :: STGroup String -> Page -> String
buildPage template page@(Page file title content published) =
    let
        attributes = [("content", [content]), 
                      ("title", [title]), 
                      ("published", [formatDate published])]
        templateAttr = setManyAttrib attributes (fromJust $ getStringTemplate "index" template )
    in
       render templateAttr


getFilesR :: String -> IO [String]
getFilesR path = do
  e <- doesDirectoryExist path
  if e
       then getDirectoryContents path >>= filterM (return.not.(flip elem ['.', '_']).head)  >>= mapM (getFilesR.(path </>)) >>= ((return.join :: [[a]] -> IO [a]))
       else return [path]

-- gets the attributs required by the main blog page (i.e. 'Posts') and the Atom feed
getBlogAttribs posts = [
 ("updated", [formatDateAtom $ pagePublished (last posts)]),
 ("updatedAtom", [formatDateAtom $ pagePublished (last posts)]),
 ("postTitles", (map pageTitle posts)),
 ("postContents", (map pageContent posts)),
 ("postDates", map (formatDate.pagePublished) posts),
 ("postDatesAtom", map (formatDateAtom.pagePublished) posts),
 ("postAddresses", map ((++ ".html").pageFile) posts)]

setContentTemplate group tname = mergeSTGroups newContentGroup group
    where newContentGroup = groupStringTemplates [("content", fromJust $ getStringTemplate tname group)]

buildBlog :: STGroup String -> FilePath -> FilePath -> IO ()
buildBlog templates postsDir outDir = 
    let postTemplates = setContentTemplate templates "post"
        blogTemplate = fromJust $ 
                       getStringTemplate "index" $ 
                       setContentTemplate templates "blog"
        feedTemplate = fromJust $ getStringTemplate "feed" templates
    in
    do
      posts <- getDirectoryContents postsDir >>= 
               filterM (return.isPageFile) >>= 
               mapM (loadPage.(postsDir </>)) >>= 
               return.(sortBy (\ p1 p2 -> (pagePublished p1) `compare` (pagePublished p2)))
      createDirectoryIfMissing True (outDir </> "posts")
      -- build each post
      mapM (\ p -> writeFile 
            (outDir </> "posts" </> (dropExtension (pageFile p)) ++ ".html")
            (buildPage postTemplates p)) posts
      -- build the index
      writeFile (outDir </> "blog.html") (render $ setManyAttrib (getBlogAttribs (reverse posts)) blogTemplate)
      -- build the feed
      writeFile (outDir </> "feed.xml") (render $ setManyAttrib (getBlogAttribs (reverse $ take 10 posts)) feedTemplate)
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
  mapM (\ p -> do
          page <- loadPage p
          outFile <- return (outDir </> makeRelative dir (dropExtensions p) ++ ".html")
          createDirectoryIfMissing True (takeDirectory outFile)
          html <- return $ buildPage (setContentTemplate templates "page") page
          writeFile outFile html) pages
  -- generate posts, post list and atom feed
  doesDirectoryExist (dir </> "_posts") >>= buildBlog templates (dir </> "_posts") outDir ?? return ()

  return ()

main = buildSite "."