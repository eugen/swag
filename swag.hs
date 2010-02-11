module Main
where
import Prelude
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock
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


(~>) = flip ($)
infixl 1 ~>

getDirectoryContentsEx dir = 
    getDirectoryContents dir >>= 
    return . filter (not.(== '.').head) >>= 
    partitionM (\ d -> doesDirectoryExist (dir </> d))

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = foldM (\ (l,r) e -> return . ((e:l,r) ?? (l,e:r)) =<< f e) ([],[]) $ reverse xs

data Page = Page {
      pageFile :: String, 
      pageTitle :: String,
      pageContent :: String, 
      pagePublished :: UTCTime 
    }

data PageDir = PageDir {
      dirPath :: String,
      dirName :: String,
      dirPages :: [Page], 
      dirTemplates :: STGroup String,
      dirChildren :: [PageDir] 
    }

pageFiles files = 
    filter ((== ".pandoc").takeExtension) files

commitDate :: String -> IO UTCTime
commitDate path = do
  oldcd <- getCurrentDirectory
  setCurrentDirectory $ takeDirectory path
  (exitCode, dateStr, stdErr) <- readProcessWithExitCode "git" ["log", "--format=%at", "-1", (takeFileName path)] ""
  setCurrentDirectory oldcd
  maybe getCurrentTime return (parseTime defaultTimeLocale "%s" dateStr)

getPageTitle content = 
    if take 2 title == "% " then drop 2 title else "Untitled"
        where title = head $ lines content
            
loadPage :: String -> IO Page
loadPage path = do
  content <- readFile path
  published <- commitDate path
  fileName <- return (dropExtension $ takeFileName path)
  return $ Page fileName (getPageTitle content) ((writeHtmlString defaultWriterOptions . readMarkdown defaultParserState) content) published

loadContent :: String -> STGroup String -> IO PageDir 
loadContent path parentTemplates = do
  dirName <- return $ takeFileName path
  (subDirs,pageFiles) <- getDirectoryContentsEx path >>= (\ (subdirs,files) -> return (subdirs,pageFiles files))
  pages <- mapM (loadPage.(path </>)) pageFiles
  ownTemplates <- doesDirectoryExist (path </> "templates") >>= directoryGroup (path </> "templates") ?? return nullGroup
  templates <- return $ mergeSTGroups ownTemplates parentTemplates
  children <- mapM (flip loadContent templates) (map (path </>) subDirs)
  return (PageDir path dirName pages templates children)

buildSite :: String -> IO ()
buildSite dir = do
  content <- loadContent dir nullGroup
  buildDir "/" content
  return ()

buildPage templates attributes page@(Page file title content published) =
    let
        templates' = maybe templates
                     (\ t -> mergeSTGroups (groupStringTemplates [("content", t)]) templates)
                     (getStringTemplate' file templates)
        template = fromJust (getStringTemplate "index" templates')
        attributes' = attributes ++ [("content", [content]), ("title", [title]), ("published", [show published])]
        templateAttr = setManyAttrib attributes' template 
    in
       render templateAttr

buildDir :: String -> PageDir -> IO ()
buildDir relPath (PageDir path dir pages templates children) =
    let attributes = [
         ("relPath", [relPath]),
         ("children", [dirName c | c <- children]),
         ("pages", map pageFile pages)]
    in do
      mapM_ (\ p@(Page {pageFile=file}) -> writeFile (path </> file ++ ".html") (buildPage templates attributes p)) pages
      mapM_ (\c -> buildDir (relPath ++ dirName c ++ "/") c) children
         
