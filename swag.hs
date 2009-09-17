module Main
where
import Prelude
import System.Environment
import Data.List

main :: IO ()
main = do
       args <- getArgs
       mapM_ putStr ("Teh args: " : (intersperse ", " args))
