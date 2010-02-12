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


(~>) = flip ($)
infixl 1 ~>

capitalize = tail.concat.map (\ w -> ' ' : (toUpper.head $ w) : (tail w)).words

emptyGroup :: STGroup String 
emptyGroup = groupStringTemplates []

getDirectoryContentsEx dir = 
    getDirectoryContents dir >>= 
    return . filter (not.(== '.').head) >>= 
    partitionM (\ d -> doesDirectoryExist (dir </> d))

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = foldM (\ (l,r) e -> return . ((e:l,r) ?? (l,e:r)) =<< f e) ([],[]) $ reverse xs

a >>> b = (unsafePerformIO $ putStrLn $ show a) `seq` b

ordinalize :: (Integral a) => a -> String
ordinalize n 
    | elem n [11..13] = show n ++ "th"
    | n `mod` 10 == 1 = show n ++ "st"
    | n `mod` 10 == 2 = show n ++ "nd"
    | n `mod` 10 == 3 = show n ++ "rd"

prettyDate :: UTCTime -> String
prettyDate date@(UTCTime day seconds) = 
    pd date dDays dHours dMinutes
    where 
      now@(UTCTime nowDay nowSeconds) = unsafePerformIO getCurrentTime
      dDays = diffDays nowDay day
      (dHours, dMinutes) = divMod (floor $ toRational ((nowSeconds - seconds) / 60)) 60
      pd date d h m 
          | d == 0 && h == 0 && m < 5 = "just posted!"
          | d == 0 && h == 0 = (show m) ++ " minutes ago"
          | d == 0 = show h ++ " hour" ++ ("s " ?? " ") (h > 1) ++ show m ++ " minutes ago"
          | d == 1 = "yesterday"
          | d < 5 = show d ++ " days ago"
          | otherwise = formatTime defaultTimeLocale ("%e %B %Y") date

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
  return $ Page 
         fileName 
         (getPageTitle content) 
         ((writeHtmlString defaultWriterOptions . readMarkdown defaultParserState) content) 
         published

loadContent :: String -> STGroup String -> IO PageDir 
loadContent path parentTemplates = do
  dirName <- return $ takeFileName path
  (subDirs,pageFiles) <- getDirectoryContentsEx path >>= (\ (subdirs,files) -> return (subdirs,pageFiles files))
  pages <- mapM (loadPage.(path </>)) pageFiles
  ownTemplates <- doesDirectoryExist (path </> "templates") >>= directoryGroup (path </> "templates") ?? return emptyGroup
  templates <- return $ mergeSTGroups ownTemplates parentTemplates
  children <- mapM (flip loadContent templates) (map (path </>) subDirs)
  return (PageDir path dirName pages templates children)

buildSite :: String -> IO ()
buildSite dir = do
  content <- loadContent dir emptyGroup
  buildDir "/" content
  return ()

buildPage templates attributes page@(Page file title content published) =
    let
        templates' = maybe templates
                     (\ t -> mergeSTGroups (groupStringTemplates [("content", t)]) templates)
                     (getStringTemplate file templates)
        template = fromJust (getStringTemplate "index" templates')
        attributes' = attributes ++ [
                       ("content", [content]), 
                       ("title", [title]), 
                       ("published", [prettyDate published])]
        templateAttr = setManyAttrib attributes' template 
    in
       render templateAttr

buildDir :: String -> PageDir -> IO ()
buildDir relPath (PageDir path dir pages templates children) =
    let attributes = [
         ("relPath", [relPath]),
         ("children", [dirName c | c <- children]),
         ("pages", map pageFile pages)] ++ [
         ("children" ++ capitalize childName ++ "Contents", map pageContent childPages) | PageDir{dirName=childName,dirPages=childPages} <- children] ++ [
         ("children" ++ capitalize childName ++ "Titles", map pageTitle childPages) | PageDir{dirName=childName,dirPages=childPages} <- children]
        writePage p@(Page {pageFile=file}) = writeFile (path </> file ++ ".html") (buildPage templates attributes p)
    in do
      mapM_ writePage pages
      mapM_ (\c -> buildDir (relPath ++ dirName c ++ "/") c) children

         
