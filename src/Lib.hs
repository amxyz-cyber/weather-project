{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards,BlockArguments,ScopedTypeVariables,FlexibleContexts,TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-missing-signatures -Wall #-}

-- exporting functions
module Lib
    ( 
           MyWeatherProject(..)
         , ToDo(..)  
         , Flag(..)
         , MyData
         , mymovingAVGTime
         , getMyCSV
         , modifyTVar_
         , doesNameExist
         , setFilePath
         , getTVar
         , parsecsv'
         , parsecsv''
         , loopThroughData
         , writeToCSV
         , getDiagram
         , getString
         , toString
         , parseArgs
         , getPair
         , setArguments
    ) where

import System.Environment (getArgs)    
import System.IO
import System.FilePath
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified System.Console.GetOpt as SCG
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU 
import qualified Graphics.Vega.VegaLite as VL
import Paths_weather_project (version)
import Data.Version (showVersion)
import System.Exit

                         
{- redifinition of getOpts stuff:

data OptDescr a = Option [Char] [String] (ArgDescr a) String
data Flag = Help | CSV String String | Chart String String | T deriving Eq 
            
data ArgDescr a = NoArg a | ReqArg ((String,String) -> a) String | OptArg (Maybe String -> a) String -}

-- defining datatypes and type synonyms
data Flag = Verbose | Version | Help | Quit | CSV String | Chart String | Test | Region String deriving (Eq , Show)
data ToDo a = TestData | UserData a | UserChart a deriving (Eq,Show)
type CSVPath = FilePath
type HtmlPath = FilePath
type CSVLink = T.Text
type MyData = T.Text
type MyCSVPath = TVar CSVPath
type MyHtmlPath = TVar HtmlPath
type MyRegion = TVar MyData
type MyLink = TVar CSVLink
type Temperature = Float
type STemperature = T.Text
type DataHeader = BS.ByteString
type Year = Int
data MTemperature = TJust Temperature | TNothing deriving (Show,Eq)
data AVGTemperature = Global MTemperature | Milan MTemperature deriving (Show,Eq)
data WeatherData = WeatherData
    {     year   :: !Year
        , temperature :: !STemperature
    }
    
data WeatherComparison = WeatherComparison
    {     yearComparison   :: Year
        , temperatureMovAVG :: Temperature
        , dataCategory :: MyData
    }

type DataCollection = Map.Map Year MTemperature
type MyDataCollection = TVar DataCollection
type AvgDataCollection = Map.Map Year (AVGTemperature,AVGTemperature)
type AvgUserDataCollection = Map.Map Year (MTemperature,Maybe MyData)
type MyMovingAVGCollection = TVar AvgDataCollection
type UserMovingAVGCollection = TVar AvgUserDataCollection
data MyWeatherProject = MyWeatherProject
    {
          pathCSVGlobal :: MyCSVPath
        , pathCSVMilan  :: MyCSVPath
        , pathMyCSV  :: MyCSVPath
        , pathCSV :: CSVPath
        , pathMyChart :: MyHtmlPath
        , linkCSVFile :: MyLink
        , region1 :: MyRegion
        , region2 :: MyRegion
        , globalData :: MyDataCollection 
        , milanData :: MyDataCollection
        , avgData :: MyMovingAVGCollection
        , avgUserData :: UserMovingAVGCollection
        , xAxisStep :: Year
        , movingAverageTime :: Year
    }

-- defining some constant variables
emptyMilan :: DataCollection
emptyMilan = Map.empty

emptyGlobal :: DataCollection
emptyGlobal = Map.empty    

empty :: AvgDataCollection
empty = Map.empty  

emptyUsermap :: AvgUserDataCollection
emptyUsermap = Map.empty  

mychartfile :: HtmlPath
mychartfile = "moving-averages.html"
    
csvGlobal :: CSVPath
csvGlobal = "results-global.csv"

csvMilan :: CSVPath
csvMilan = "results-milan.csv"

myCsvFile :: CSVPath
myCsvFile = "moving-average-weather-data.csv"

mypath :: CSVPath
mypath = "data"

mymovingAVGTime :: Year
mymovingAVGTime = 10

mystep :: Year
mystep = 5

myCsvLink :: CSVLink
myCsvLink = "https://raw.githubusercontent.com/amxyz-cyber/data/8042057626ab000a5fc83ea95ac7d770570853fc/moving-average-weather-data.csv"

yearHeader :: DataHeader
yearHeader = BL.toStrict $ BLU.fromString "year"

dataHeader :: DataHeader
dataHeader = BL.toStrict $ BLU.fromString "data"

maHeader :: DataHeader
maHeader = BL.toStrict $ BLU.fromString $ (show mymovingAVGTime) ++ "-" ++ BLU.toString (BL.fromStrict yearHeader) ++ "_MA"

milanT :: MyData
milanT = "milan"

globalT :: MyData
globalT = "global"

regionA :: MyData
regionA = "region1"

regionB :: MyData
regionB = "region2"

-- initializing the datatype MyWeatherProject which holds variables to 
-- store the paths and other stuff for creating the moving averages
-- and plotting the diagram
getMyCSV :: IO MyWeatherProject
getMyCSV = do 
      tstore <- atomically $ newTVar csvGlobal
      tstore2 <- atomically $ newTVar csvMilan
      tstore6 <- atomically $ newTVar myCsvFile
      tstore7 <- atomically $ newTVar mychartfile
      tstore3 <- atomically $ newTVar emptyGlobal
      tstore4 <- atomically $ newTVar emptyMilan
      tstore5 <- atomically $ newTVar empty
      tstore8 <- atomically $ newTVar myCsvLink
      tstore9 <- atomically $ newTVar globalT
      tstore10 <- atomically $ newTVar milanT
      tstore11 <- atomically $ newTVar emptyUsermap
      
      return MyWeatherProject {
                 pathCSVGlobal = tstore
               , pathCSVMilan = tstore2
               , pathCSV = mypath
               , pathMyCSV = tstore6
               , pathMyChart = tstore7
               , globalData =  tstore3
               , milanData = tstore4
               , avgData = tstore5
               , avgUserData = tstore11
               , xAxisStep = mystep
               , linkCSVFile = tstore8
               , region1 = tstore9
               , region2 = tstore10
               , movingAverageTime = mymovingAVGTime }

-- modifies a tv variable    
modifyTVar_ :: TVar a -> a -> IO ()
modifyTVar_ tv newVal = do
    atomically $ writeTVar tv newVal

-- gets the variable stored in a tv variable
getTVar :: TVar b -> IO b
getTVar tv  = do
    store <- atomically $ readTVar tv
    return store

-- checks if a file exists
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
      
-- sets the filepath
setFilePath :: FilePath -> FilePath -> FilePath
setFilePath dir f = dir </> f

-- parses the arguments of the command line tool
parseArgs :: IO (ToDo [Flag], Maybe Flag)
parseArgs = do
    argv <- getArgs
    case parse argv of
        ([],_,[]  )                         -> return $ (TestData,Nothing)
        (opts, _, [])                       -> f (nub opts) 
        (_,_,errs)                          -> die' errs
    where
        parse argv = SCG.getOpt SCG.Permute options argv
        optHeader     = "Usage: weatherComparator [-h] [-c csv-file1] [-c csv-file2] [-d link] [-t] [-r region1] [-r region2]  "
        info       = SCG.usageInfo optHeader options
        dump       = hPutStrLn stderr
        die' errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
        help       =  dump info                  >> exitWith ExitSuccess
        quit       = putStrLn "!Goodbye!" >> exitWith ExitSuccess
        printVersion = putStrLn (showVersion version) >> exitWith ExitSuccess
        f opts 
          | Help `elem` opts  = help 
          | Version `elem` opts = printVersion
          | Quit `elem` opts  = quit
          | Test `elem` opts && Verbose `elem` opts = return $ (TestData, Just Verbose)
          | Test `elem` opts = return $ (TestData,Nothing)
          | otherwise = checkAllOptions opts help

-- checks if the flag is of type Region
isRegion :: Flag -> Bool
isRegion (Region _) = True
isRegion _         = False

-- checks if the flag is of type CSV
isCSV :: Flag -> Bool
isCSV (CSV _) = True
isCSV _         = False

-- checks if the flag is of type Chart
isChart :: Flag -> Bool
isChart (Chart _) = True
isChart _         = False
        
-- checks how many times a flag has been entered
checkOptions :: [a] -> Int -> [a]
checkOptions [] _ = []
checkOptions l n
    | length l == n  = l
    | otherwise = []

-- retrieves the value which has been entered together with the flag
getItem :: Flag -> Maybe String
getItem (Region y) = Just y
getItem (CSV y) = Just y
getItem (Chart y) = Just y
getItem _ = Nothing

-- determines what the user wants
checkAllOptions :: Monad m => [Flag] -> m (ToDo [Flag], Maybe Flag) -> m (ToDo [Flag], Maybe Flag)
checkAllOptions opts h = do
    let lenR = 2
    let lenCSV = 2
    let lenCSV2 = 1
    let lenC = 1
    let regions = checkOptions (filter isRegion opts) lenR
    let csv = checkOptions (filter isCSV opts)  lenCSV
    let csv2 = checkOptions (filter isCSV opts)  lenCSV2
    let chart = checkOptions (filter isChart opts)  lenC
    let vb = Verbose `elem` opts
    case (length regions == lenR,length csv == lenCSV,length csv2 == lenCSV2,length chart == lenC,vb) of
        (True,True,_,_,True) -> return $ (UserData (csv ++ regions),Just Verbose)
        (True,True,_,_,_) -> return $ (UserData (csv ++ regions),Nothing)
        (True,_,True,True,True) -> return $ (UserChart (chart ++ regions++csv2),Just Verbose)
        (True,_,True,True,_) -> return $ (UserChart (chart ++ regions++csv2),Nothing)
        (_,True,_,_,True) -> return $ (UserData (csv),Just Verbose)
        (_,True,_,_,_) -> return $ (UserData csv,Nothing)
        (_,_,True,True,True) -> return $ (UserChart (chart++csv2),Just Verbose)
        (_,_,True,True,_) -> return $ (UserChart (chart++csv2),Nothing)
        (_,_,_,_,_) -> h

-- defines the options which the user can enter                         
options :: [SCG.OptDescr Flag]
options =
    [ SCG.Option ['v']     ["verbose"] (SCG.NoArg Verbose)           "chatty output on stderr"
    , SCG.Option ['V','?'] ["version"] (SCG.NoArg Version)           "show version number"
    , SCG.Option ['q']     ["quit"]   (SCG.NoArg Quit)               "end program"
    , SCG.Option ['c']     ["csv"]     (SCG.ReqArg CSV "FILES")      "csv file for temperature comparison"
    , SCG.Option ['d']     ["chart"]   (SCG.ReqArg Chart "LINK")     "create a diagram"
    , SCG.Option ['t']     ["test"]    (SCG.NoArg Test)              "create a diagram based on test set"
    , SCG.Option ['r']     ["region"]  (SCG.ReqArg Region "REGIONS") "the name of a region"
    , SCG.Option ['h']     ["help"]    (SCG.NoArg Help)              "Show this help message"
    
    ]

-- overwrites the regions with the standard values if the user
-- hasn't entered them
setRegions :: [Flag] -> MyWeatherProject -> IO ()
setRegions [] mydata = do
    let MyWeatherProject{region1=tvReg1,region2=tvReg2} = mydata
    modifyTVar_ tvReg1 regionA
    modifyTVar_ tvReg2 regionB
    return ()

-- overwrites the regions with the entered values of the user   
setRegions (x:xs) mydata = do
    let MyWeatherProject{region1=tvReg1,region2=tvReg2} = mydata
    let mx = getItem x
    let mtail = getItem (head xs)
    case (mx,mtail) of
        (Just x1, Just mytail) -> do 
                    modifyTVar_ tvReg1 (T.pack x1)
                    modifyTVar_ tvReg2 (T.pack mytail)
                    return ()
        _ -> return ()

-- overwrites the csv file paths with the entered values of the user        
setCSVs :: [Flag] -> MyWeatherProject -> IO ()
setCSVs [] _ = return ()    
setCSVs (x:xs) mydata = do
    let MyWeatherProject{pathCSVGlobal=tvC1,pathCSVMilan=tvC2 } = mydata
    let mx = getItem x
    let mtail = getItem ( head xs)
    case (mx,mtail) of
        (Just x1, Just mytail) -> do
                    modifyTVar_ tvC1 x1
                    modifyTVar_ tvC2 mytail
                    return ()
        _ -> return ()

-- overwrites the path of the csv file containing the moving averages
setCSV :: [Flag] -> MyWeatherProject -> IO ()
setCSV [] _ = return () 
setCSV (x:_) mydata = do
    let MyWeatherProject{pathMyCSV= tvMycsv } = mydata
    let mx = getItem x
    case mx of
        Just x1 -> modifyTVar_ tvMycsv x1 >> return ()
        _ -> return ()

-- overwrites the link to the csv file with the entered value of the user   
setChart :: [Flag] -> MyWeatherProject -> IO ()
setChart [] _ = return ()
setChart (x:[]) mydata = do
    let MyWeatherProject{linkCSVFile=tvLink} = mydata
    let mx = getItem x
    case mx of
        Just x1 -> modifyTVar_ tvLink (T.pack x1) >> return ()
        _ -> return ()

-- stores the entered arguments of the user
setArguments :: [Flag] -> MyWeatherProject -> IO ()
setArguments l mydata = do
    let regions = filter isRegion l
    let chart = filter isChart l
    let csv = filter isCSV l
    let lenCSV = length csv
    case lenCSV of
        1 -> do
            setRegions regions mydata
            setCSV csv mydata
            setChart chart mydata
        _ -> do
            setRegions regions mydata
            setCSVs csv mydata
            setChart chart mydata
    
-- calculates the sum of a series of temperatures
mysum :: Foldable t => t (a, MTemperature) -> Temperature
mysum xs = foldl(\acc (_,TJust y) -> acc+y) 0 xs

-- calculates the average temperature
avg :: [(a, MTemperature)] -> Temperature
avg xs = (mysum xs ) / (genericLength xs)

-- converts a text variable to MTemperature
toMTemperature :: WeatherData -> MTemperature
toMTemperature (WeatherData{temperature=t}) 
    | T.length t > 0 = TJust $ convert t
    | otherwise = TNothing

-- converts a float to MTemperature
toMTemperature2 :: WeatherComparison -> MTemperature
toMTemperature2 (WeatherComparison{temperatureMovAVG=t}) = TJust t
    
-- converts a string to Temperature
convert :: STemperature -> Temperature
convert x = read (T.unpack x)

-- converts a text to Maybe MyData
toMData :: WeatherComparison -> Maybe MyData
toMData (WeatherComparison{dataCategory=t}) 
    | T.length t > 0 = Just t
    | otherwise = Nothing
        
-- inserts a row of a csv file into a map
insertTemperature :: Year -> MTemperature -> DataCollection -> DataCollection
insertTemperature y mt map' = case Map.lookup y map' of
    Nothing -> Map.insert y mt map'
    _ -> map'
    
-- inserts a row of a csv file containing the moving averages into a map
insertUserTemperature :: Year -> MTemperature -> Maybe MyData -> AvgUserDataCollection -> AvgUserDataCollection
insertUserTemperature y mt mcat map' = case Map.lookup y map' of
    Nothing -> Map.insert y (mt,mcat) map'
    _ -> map'    
    
-- inserts moving averages for one region into a map
insertGlobalAvgTemperature :: Year -> MTemperature -> AvgDataCollection -> AvgDataCollection
insertGlobalAvgTemperature y mt map' = case Map.lookup y map' of
    Nothing -> Map.insert y (Global mt,Milan TNothing) map'
    Just (Global _,Milan mmt) -> Map.insert y (Global mt,Milan mmt) map'
    Just (Global _,_) -> Map.insert y (Global mt,Milan TNothing) map'
    Just (_,Milan mmt) -> Map.insert y (Global mt,Milan mmt) map'
    _ -> map'
    
-- inserts moving averages for another region into a map
insertMilanAvgTemperature :: Year -> MTemperature -> AvgDataCollection -> AvgDataCollection
insertMilanAvgTemperature y mt map' = case Map.lookup y map' of
    Nothing -> Map.insert y (Global TNothing,Milan mt) map'
    Just (Global mgt,Milan _) -> Map.insert y (Global mgt,Milan mt) map'
    Just (Global mgt,_) -> Map.insert y (Global mgt,Milan mt) map'
    Just (_,Milan _) -> Map.insert y (Global TNothing,Milan mt) map'
    _ -> map'
 
-- gets the largest value of a map
setMax :: Ord a => Map.Map a b1 -> Map.Map a b2 -> a
setMax map1 map2
    | auxMax map1 < auxMax map2 = auxMax map1
    | otherwise = auxMax map2
    where 
        auxMax map' = getPair $ Map.findMax map'

-- gets the smallest value of a map
setMin :: Ord a => Map.Map a b1 -> Map.Map a b2 -> a
setMin map1 map2
    | auxMin map1 > auxMin map2 = auxMin map1
    | otherwise = auxMin map2
    where 
        auxMin map' = getPair $ Map.findMin map'

-- gets the first item of a tuple
getPair :: (a, b) -> a
getPair (g,_) = g         
        
-- gets the next upper border for which to calculate the moving average
getNextMax :: Year -> Year
getNextMax max' = max' + mymovingAVGTime - 1    

-- gets the next lower border for which to calculate the moving average
getNextMin :: Enum a => a -> a
getNextMin min' = succ(min')

-- increases the maximum year by one
getNextMax2 :: Enum a => a -> a
getNextMax2 max' = succ(max')

-- loops through the two maps which contain the values of the two regions
loopThroughData :: TVar (Map.Map Year MTemperature) -> TVar (Map.Map Year MTemperature) -> TVar AvgDataCollection -> IO ()
loopThroughData tstore tstore2 tstore3 = do
    storeG <- getTVar tstore
    storeM <- getTVar tstore2
    
    let min' = setMin storeG storeM
        max' = setMax storeG storeM
        iMax = getNextMax min'
        isGlobal = True
        isMilan = False
    loop min' iMax max' isGlobal storeG tstore3 
    loop min' iMax max' isMilan storeM tstore3 
    return ()
    
    where
        loop iMin iMax max' flag map' mytstore = do
            map2 <- getTVar mytstore
            let isOK = checkInterval iMax max'
            (mtemp,iMax') <- getMovingAVG map' iMin iMax max'
            case (isOK,mtemp,flag) of
                (False,_,_) -> return ()
                (_,TNothing,_) -> return ()
                (_,TJust _,True) -> do
                    let map2' = insertGlobalAvgTemperature iMax' mtemp map2
                        newMin = getNextMin iMin
                        newMax = getNextMax2 iMax'
                    atomically $ writeTVar mytstore map2'
                    loop newMin newMax max' flag map' mytstore
                (_,TJust _,_) -> do
                    let map2' = insertMilanAvgTemperature iMax' mtemp map2
                        newMin = getNextMin iMin
                        newMax = getNextMax2 iMax'
                    atomically $ writeTVar mytstore map2'
                    loop newMin newMax max' flag map' mytstore
                                            
        checkInterval iMax max'
            | iMax <= max' = True
            | otherwise = False

-- sets the current moving average together with the corresponding year
getMovingAVG :: (Monad m, Ord b, Enum b) => Map.Map b MTemperature -> b -> b -> b -> m (MTemperature, b)
getMovingAVG map' mymin mymax max' = do
    (mmap,iMax') <- loop map' mymin mymax max'
    case mmap of
            (Just newMap) -> do
                let list = Map.toList newMap
                    currentAVG = avg list
                return $ (TJust currentAVG,iMax')
            _ -> return $ (TNothing,iMax')
        
    where
        loop mymap iMin iMax maxKey = do
            let newMap = getFilteredList mymap iMin iMax 
                len = Map.size newMap
                hasEnoughItems = isLongEnough len
                mmap = getNewMap hasEnoughItems newMap
            case(max' >= maxKey, mmap) of
                (True,_) -> return (mmap,iMax)
                (_, Just _) -> return (mmap,iMax)
                (_,_) -> do
                    let newMax = succ(iMax)
                    loop mymap iMin newMax maxKey
        
        getFilteredList mymap iMin iMax =  Map.filterWithKey (\k v -> k >= iMin && k <= iMax && v /= TNothing ) mymap
        
        isLongEnough len 
            | len == mymovingAVGTime = True
            | otherwise = False
        
        getNewMap hasEnoughItems newMap
            | hasEnoughItems = Just newMap
            | otherwise = Nothing
            

-- converts the map containing the calculated moving averages of one region into a list
createMilanDataList :: AvgDataCollection -> MyData -> [WeatherComparison]
createMilanDataList map1 reg = map (\(key,(Global (TJust _),Milan (TJust m))) -> WeatherComparison{yearComparison=key,temperatureMovAVG=m,dataCategory=reg} ) (getList map1)
    where
        getList :: AvgDataCollection -> [(Year,(AVGTemperature,AVGTemperature))]
        getList map' = Map.toAscList map'

-- converts the map containing the calculated moving averages of the 
-- other region into a list        
createGlobalDataList :: AvgDataCollection -> MyData -> [WeatherComparison]
createGlobalDataList map1 reg = map (\(key,(Global (TJust g),Milan (TJust _))) -> WeatherComparison{yearComparison=key,temperatureMovAVG=g,dataCategory=reg} ) (getList map1)
    where
        getList :: AvgDataCollection -> [(Year,(AVGTemperature,AVGTemperature))]
        getList map' = Map.toAscList map'

-- converts integer to double
mydouble :: Int -> Double
mydouble x = a
  where a = fromIntegral x :: Double


-- contains the headings of the moving average csv file
itemHeader :: Header
itemHeader = V.fromList [ yearHeader, maHeader , dataHeader ]

-- overwrites the instance FromNamedRecord for moving averages csv files
instance FromNamedRecord WeatherComparison where
    parseNamedRecord r = WeatherComparison <$> r .: yearHeader <*> r .: maHeader <*> r .: dataHeader
    
-- parses a csv file already containing moving average temperatures
parsecsv'' :: BLU.ByteString -> TVar AvgUserDataCollection -> IO ()
parsecsv'' c tstore = do
  case decodeByName c of
            Left err -> putStrLn err
            Right (_, v) -> V.forM_ v $ \ w -> do
                    store <- getTVar tstore 
                    let mt = toMTemperature2 w
                    let mcat = toMData w
                    let map1 = insertUserTemperature (yearComparison w ) mt mcat store
                    atomically $ writeTVar tstore map1

-- overwrites the instance FromNamedRecord for csv files
instance FromNamedRecord WeatherData where
    parseNamedRecord r = WeatherData <$> r .: "year" <*> r .: "avg_temp"

-- parses a csv file containing a collection of temperatures    
parsecsv' :: BLU.ByteString -> TVar DataCollection -> IO ()
parsecsv' c tstore = do
  case decodeByName c of
            Left err -> putStrLn err
            Right (_, v) -> V.forM_ v $ \ w -> do
                store <- getTVar tstore     
                let mt = toMTemperature w
                    map1 = insertTemperature (year w ) mt store
                atomically $ writeTVar tstore map1

-- overwrites the instance ToNamedRecord for writing the calculated 
-- moving averages to the new csv file
instance ToNamedRecord WeatherComparison where
  toNamedRecord WeatherComparison{..} =
    namedRecord
      [ yearHeader .= yearComparison
      , maHeader .= temperatureMovAVG
      , dataHeader .= dataCategory
      ]
      
-- writes the calculated moving averages into a csv file
writeToCSV :: FilePath -> MyMovingAVGCollection -> MyRegion -> MyRegion -> IO () 
writeToCSV fileName tstore tstore2 tstore3= do
    store <- getTVar tstore
    reg1 <- getTVar tstore2
    reg2 <- getTVar tstore3
    let dataList = (createGlobalDataList store reg1) ++ (createMilanDataList store reg2)
        bs = encodeByName itemHeader dataList
    BL.writeFile fileName bs
    
-- converts Text to String
getString :: T.Text -> String
getString t = T.unpack t

-- converts CSVPath to String
toString :: CSVPath -> String
toString s = s

-- creates a htmlfile of the diagram
getDiagram :: FilePath -> Int -> Int -> Int -> T.Text -> T.Text -> IO ()
getDiagram htmlFile' iMin iMax step link title = do
    VL.toHtmlFile htmlFile' $ createDiagram iMin iMax step link title
    return ()

-- plots the diagram using vega lite
createDiagram :: Int -> Int -> Int -> T.Text -> T.Text -> VL.VegaLite
createDiagram min' max' step' link title = 
    let iMax = mydouble max'
        iMin = mydouble min'
        step = mydouble step'
        xVal = T.pack(BLU.toString (BL.fromStrict(yearHeader )))
        yVal = T.pack(BLU.toString (BL.fromStrict(maHeader )))
        cols = T.pack(BLU.toString (BL.fromStrict(dataHeader )))
        xValUpper = T.toUpper xVal
        colsUpper = T.toUpper cols

        axis = VL.PAxis [ VL.AxValues (VL.Numbers [ iMin,iMin+step.. iMax ]) ]
        bkg = VL.background "rgba(0, 0, 0, 0.05)"
        enc = VL.encoding 
                . VL.position VL.X [ VL.PName xVal, VL.PmType VL.Temporal ,axis, VL.PAxis [ VL.AxTitle xValUpper ] ] 
                . VL.position VL.Y [  VL.PName yVal, VL.PmType VL.Quantitative, VL.PScale [ VL.SZero False ], VL.PAxis [ VL.AxTitle "Temperature in Celcius" ]]
                . VL.color [ VL.MName cols, VL.MmType VL.Nominal,VL.MLegend [ VL.LTitle colsUpper], VL.MScale [ VL.SScheme "viridis" []] ]
        
    in VL.toVegaLite 
       [ 
       bkg
       , VL.mark VL.Line []
       , enc []
       , VL.dataFromUrl link [VL.CSV]
       , VL.width 1600
       , VL.height 800
       , VL.title title []
       ]
