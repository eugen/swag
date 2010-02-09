module Main
where
import Prelude
import System.Environment
import System.Directory
import System.FilePath 
import System.Process (readProcess, readProcessWithExitCode)
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock
import System.Locale
import Text.StringTemplate
import Control.Monad
import Text.Pandoc
import Data.Bool.Higher
import System

(~>) = flip ($)
infixl 1 ~>

getDirectoryContentsEx dir = 
    getDirectoryContents dir >>= 
    return . filter (not.(== '.').head) >>= 
    partitionM (\ d -> doesDirectoryExist (dir </> d))

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = foldM (\ (l,r) e -> f e >>= return . ((e:l,r) ?? (l,e:r))) ([],[]) $ reverse xs

data Page = Page {
      fileName :: String, 
      htmlContent :: String, 
      published :: UTCTime 
    }

data PageDir = PageDir {
      dirName :: String, 
      pages :: [Page], 
      templates :: STGroup String,
      children :: [PageDir] 
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

loadPage :: String -> IO Page
loadPage path = do
  content <- readFile path >>= return . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState
  published <- commitDate path
  fileName <- return (dropExtension $ takeFileName path)
  return $ Page fileName content published

loadContent :: String -> STGroup String -> IO PageDir 
loadContent path parentTemplates = do
  dirName <- return $ takeFileName path
  (subDirs,pageFiles) <- getDirectoryContentsEx path >>= (\ (subdirs,files) -> return (subdirs,pageFiles files))
  pages <- mapM (loadPage.(path </>)) pageFiles
  ownTemplates <- doesDirectoryExist (path </> "templates") >>= directoryGroup (path </> "templates") ?? return nullGroup
  templates <- return $ mergeSTGroups ownTemplates parentTemplates
  children <- mapM (flip loadContent templates) (map (path </>) subDirs)
  return (PageDir dirName pages templates children)
                  
buildSite :: String -> IO ()
buildSite dir = do
  content <- loadContent dir nullGroup
  return ()
  
-- buildDir :: STGroup String -> String -> IO ()
-- buildDir parentTemplates dir = do
--   hasOwnTemplates <- doesDirectoryExist (dir </> "templates")
--   templates <- if hasOwnTemplates 
--                then liftM (flip mergeSTGroups parentTemplates) (directoryGroup $ dir </> "templates") 
--                else return parentTemplates
--   (subdirs,files) <- getDirectoryContentsEx dir
--   children <- mapM (\ sd -> getDirectoryContentsEx (dir </> sd) >>= (\ cs -> return $ (sd, map dropExtension $ goodFiles $ snd cs))) subdirs
--   mapM (\ file -> buildFile templates (dir </> file) children) (goodFiles files)
--   mapM (\ subdir -> buildDir templates (dir </> subdir)) subdirs
--   return ()

-- buildFile :: STGroup String -> String -> [(String, [String])]-> IO ()
-- buildFile templates file attribs = do
--     fileContent <- readFile file
--     template <- return $ fromJust $ getStringTemplate "main" templates
--     htmlFragment <- return $ writeHtmlString defaultWriterOptions $ readMarkdown defaultParserState fileContent
--     template' <- return $ setAttribute "content" htmlFragment template
--     putStrLn (show attribs)
--     template'' <- return $ setManyAttrib attribs template' --foldl (flip setManyAttrib) template' attribs
--     html <- return $ render $ template''
--     writeFile (replaceExtension file ".html") html
