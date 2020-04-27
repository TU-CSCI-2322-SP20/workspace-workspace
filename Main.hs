module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Random
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Mushroom

-- Options record
data Options = Options {
   optHelp              :: Bool
 , optQuiet             :: Bool
 , optVerbose           :: Bool
 , optPrint             :: Bool
 , optConcise           :: Bool
 , optTree              :: Bool
 , edLimit              :: Double
 , mushroomCount        :: Int
 , mushroomTest         :: Maybe String
 , fname                :: String
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optQuiet = False
    , optTree  = False
    , optPrint = False
    , optConcise = False
    , edLimit  = 0.9
    , optVerbose = False
    , mushroomTest = Nothing
    , mushroomCount = 0 
    , fname = "mushrooms.csv"
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]   (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option ['n'] []         (ReqArg (\n opts -> opts { mushroomCount = (read n) }) "n") "Use n mushrooms.",
  Option ['p'] ["print"]  (NoArg  (\opts -> opts { optPrint = True })) "Read in the mushrooms and print their representation to the screen, without building a decision tree.", 
  Option ['t'] ["tree"]   (NoArg  (\opts -> opts { optTree = True })) "Print the decision tree, without any formatting.", 
  Option ['d'] ["decide"] (ReqArg (\m opts -> opts { mushroomTest = Just $ "edible,"++m }) "m") "Decide whether or not to eat a mushroom m.",
  Option ['c'] ["concise"] (NoArg (\opts -> opts { optConcise = True }) ) "Print a concise guide without duplicate lines.",
  Option ['l'] ["limit"]  (ReqArg (\m opts -> opts { edLimit = read m }) "l") "Use l as a threshold for edibility when producing a guide.",
  Option ['v'] ["verbose"](NoArg  (\opts -> opts { optVerbose = True })) "Display more information during processing."
  ]

--TODO: should take the file name as an optional argument
-- Return the list of flags
compilerOpts :: [String] -> Options
compilerOpts argv =
  case getOpt Permute options argv of
     (o,[x],[]) -> foldl (flip id) (defaultOptions {fname = x}) o
     (o,[],[]) -> foldl (flip id) defaultOptions o
     (_,_,[]) -> error (usageInfo header options)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ./fungi [OPTION]... [file]\n\tProduces a guide for how to eat mushrooms."

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./fungi [OPTION]... [file]"

-- Main IO function
main :: IO ()
main = do
  allArgs <- getArgs
  let opts = compilerOpts allArgs
  if optHelp opts then helpIO
  else do contents <- readFile (fname opts)
          let selected = if mushroomCount opts == 0 
                         then contents 
                         else unlines $ take (1+mushroomCount opts) (lines contents)
          case readObservationFile selected of
               Just observations -> handleObservations opts observations
               Nothing -> error "readObservationFile crashed: invalid file format or invalid implementation."


showObs (m,e) = intercalate "," $ (show e):(map show m)

handleObservations :: Options -> [Observation] -> IO ()
handleObservations opts observs
  | invalidOpts   = putStrLn "Invalid flags: only one of -p, -t, and -d can be set."
  | optPrint opts = putStrLn $ unlines $ map showObs observs
  | optTree opts  = putStrLn $ fancyshow dtree
  | isJust $ m    = case readObservation $ fromJust m of
                          Nothing -> error "readObservation crashed: invalid -d flag or invalid implementation."
                          Just (mval,_) -> putStrLn $ rateMushroom mval dtree (toRational $ edLimit opts)
  | optConcise opts = putStr $ unlines $ buildGuideCSE dtree (toRational $ edLimit opts)
  | otherwise     = putStr $ unlines $ buildGuide dtree (toRational $ edLimit opts)
  where m = mushroomTest opts
        invalidOpts = (optPrint opts && optTree opts) || (isJust m && (optPrint opts || optTree opts))
        dtree = buildTree allAttributes observs

