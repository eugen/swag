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
p a = (unsafePerformIO $ putStrLn $ show a) `seq` a

ordinalize :: (Integral a) => a -> String
ordinalize n 
    | elem n [11..13] = show n ++ "th"
    | n `mod` 10 == 1 = show n ++ "st"
    | n `mod` 10 == 2 = show n ++ "nd"
    | n `mod` 10 == 3 = show n ++ "rd"

-- TODO: format dates web2.0-style (e.g. "yesterday", "3 days ago") from javascript;
formatDate :: UTCTime -> String
formatDate date = 
    formatTime defaultTimeLocale ("%e %B %Y") date

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
         ((writeHtmlString defaultWriterOptions . readMarkdown defaultParserState) content) 
         (fromMaybe commitDate' (parseTime defaultTimeLocale "%Y-%m-%d %H:%M" (fromMaybe ""  published)))
    where loadMeta content line = 
              let ls = lines content in
              if ((length ls) >= line && head (ls !! line) == '%')
              then Just (ls !! line ~> drop 1 ~> dropWhile (== ' '))
              else Nothing

buildPage :: StringTemplate String -> Page -> String
buildPage template page@(Page file title content published) =
    let
        attributes = [
                       ("content", [content]), 
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


buildSite :: String -> IO ()
buildSite dir = do
  templates <- directoryGroup (dir </> "_templates") :: IO (STGroup String)
  (pages, static) <- getFilesR dir >>= partitionM (return.isPageFile)
  outDir <- return (dir </> "_site")
  (doesDirectoryExist outDir >>= (removeDirectoryRecursive outDir) ?? (return ()))
  createDirectory outDir
  mapM (\ f ->
          let outFile = (outDir </> makeRelative dir f) in
          createDirectoryIfMissing True (takeDirectory outFile) >> copyFile f outFile) static
  indexTemplate <- return $ fromJust (getStringTemplate "index" templates)
  mapM (\ p -> do
          page <- loadPage p
          outFile <- return (outDir </> makeRelative dir (dropExtensions p) ++ ".html")
          createDirectoryIfMissing True (takeDirectory outFile)
          html <- return $ buildPage indexTemplate page
          writeFile outFile html) pages

  return ()

