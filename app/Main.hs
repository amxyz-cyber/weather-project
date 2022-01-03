{-# LANGUAGE OverloadedStrings, BlockArguments, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-missing-signatures -Wall #-}

module Main where

import qualified Lib as L
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified System.Exit as Exit
import qualified Data.Text as T

main :: IO ()
main = do
    myData <- L.getMyCSV
    res <- L.parseArgs
    checkArguments res myData                           
    
    where
        verifyT = "Verifying csv file(s) ..."++"\n"
        notFound  = "couldn't find any file(s)"
        enterPath = "please specify the path containing "
        maT = "-year Moving Averages"
        tPart1 = "Comparison of "
        tPart2 = "The Average Temprature Trends in "
        tbw = " Between "
        tAnd = " And "
        
        -- displays information regarding the file paths of the csv files
        checkFilePath p mvb t1 = case (p,mvb) of
                    (True,Just L.Verbose) -> putStrLn (verifyT++"file exists")
                    (False,Just L.Verbose) ->  Exit.die $ verifyT++enterPath++ t1
                    (False,_) -> Exit.exitWith (Exit.ExitFailure 1)
                    (_,_)    ->  return ()
        
        -- displays information regarding the file path of one csv file
        checkFilePaths p1 p2 t1 t2 mvb = case (p1,p2,mvb) of
                    (True,True,Just L.Verbose) -> putStrLn (verifyT++"both files exist")
                    (False,False,Just L.Verbose) -> Exit.die $ (verifyT++notFound)
                    (False,False,_) -> Exit.exitWith (Exit.ExitFailure 1)
                    (_,False,Just L.Verbose) ->  Exit.die $ verifyT++enterPath ++ t2
                    (_,False,_) ->  Exit.exitWith (Exit.ExitFailure 1)
                    (False,_,Just L.Verbose) ->  Exit.die $ verifyT++enterPath++ t1
                    (False,_,_) ->  Exit.exitWith (Exit.ExitFailure 1)
                    (_,_,_)     ->  return ()
                    
        -- displays information about the created csv file containg
        -- the moving averages
        checkNewFile p1 t1 fileType mvb = case (p1,mvb) of
                    (True,Just L.Verbose) -> putStrLn $ "The new " ++fileType++ " file " ++ t1 ++ " has been created."
                    (False,Just L.Verbose) -> Exit.die $ "couldn't find any files"
                    (False,_) -> Exit.exitWith (Exit.ExitFailure 1)
                    (_,_) -> return ()
        
        -- displays information about the map containing the moving
        -- averages
        checkMaps m g a tvReg1 tvReg2 mvb = do
            m' <- L.getTVar m
            g' <- L.getTVar g
            a' <- L.getTVar a
            r1 <- L.getTVar tvReg1
            r2 <- L.getTVar tvReg2
            let lenM = Map.size m'
                lenG = Map.size g'
                lenA = Map.size a'
            case (lenM>0,lenG>0,lenA>0,mvb) of
                (True,True,True,Just L.Verbose) -> putStrLn $ "Files were parsed successfully:"++"\n"++ "Length "++(T.unpack r1)++" data: "++(show lenG)++"\n"++ "Length "++(T.unpack r2)++" data: "++(show lenM)++"\n"++ "Length moving average data: "++(show lenA)
                (_,_,_,Just L.Verbose) -> Exit.die "Data could not be retrieved from files" 
                (False,False,False,_) -> Exit.exitWith (Exit.ExitFailure 1)
                (_,_,_,_) -> return ()
                    
        -- creates a title
        getTempTitle :: L.MyData -> String
        getTempTitle reg = "temperatures in " ++(T.unpack reg)
        
        auxGetTitle :: String -> String -> L.MyData
        auxGetTitle t1 t2 = T.pack $ t1++" and "++t2
        
        -- creates a title for the diagram
        getTitle iMA iMin iMax size reg1 reg2 = T.pack $ (show iMA)++maT++"\n"++tPart1++tPart2++(T.unpack $ T.toUpper reg1)++ " vs. "++(T.unpack $ T.toUpper reg2) ++"\n"++tbw++(show iMin)++tAnd++(show iMax)++"\n"++"n="++(show size)     
        getTestTitle iMA iMin iMax size = T.pack $ (show iMA)++maT++"\n"++tPart1++"The Global and "++tPart2++" Milan" ++"\n"++tbw++(show iMin)++tAnd++(show iMax)++"\n"++"n="++(show size)
        
        -- routine for reading in the csv files
        readUserData mvb t dat = do
            let L.MyWeatherProject{L.pathMyCSV=tvMycsv,L.avgUserData=tvMap,L.pathCSV=cp } = dat
            csvFile' <- L.getTVar tvMycsv
            let csvFile = L.setFilePath cp csvFile'
            existsCsvFile <- L.doesNameExist csvFile
            checkFilePath existsCsvFile mvb t
            csvAVGs <- BL.readFile csvFile
            L.parsecsv'' csvAVGs tvMap
            return ()
        
        -- routine for creating a diagram based on user data
        auxUserDiagram dat mvb l htmlT = do
            case mvb of
                    (Just L.Verbose) -> do
                        putStrLn "Creating the chart ..."
                        return ()
                    _ -> return ()
            L.setArguments l dat
            let L.MyWeatherProject{L.region1=tvReg1,L.region2=tvReg2,L.avgUserData=mapUserAVG} = dat
            reg1 <- L.getTVar tvReg1
            reg2 <- L.getTVar tvReg2
            let auxTitle = auxGetTitle (L.getString reg1) (L.getString reg2)
            let temp1 = getTempTitle auxTitle
            readUserData mvb temp1 dat
            case mvb of
                    (Just L.Verbose) -> do
                        putStrLn "successfully parsed csv file ..."
                        return ()
                    _ -> return ()
            auxDiagram dat (L.UserChart l) htmlT mvb mapUserAVG
        
        -- routine for creating a diagram 
        auxDiagram dat task fileType mvb tvmap = do
            let L.MyWeatherProject{L.pathMyChart=tchart,L.xAxisStep=step, L.linkCSVFile=tvLink,L.region1=tvReg1,L.region2=tvReg2,L.movingAverageTime=ma,L.pathCSV=cp } = dat
            link <- L.getTVar tvLink
            reg1 <- L.getTVar tvReg1
            reg2 <- L.getTVar tvReg2
            fhtml <- L.getTVar tchart
            map' <- L.getTVar tvmap
            let iMax = L.getPair $ Map.findMax map'
            let iMin = L.getPair $ Map.findMin map'
            let size = Map.size map'
            let htmlFile = L.setFilePath cp fhtml
            title <- case task of
                        L.TestData -> do
                            let title = getTestTitle ma iMin iMax size
                            return title
                        _ -> do
                            let title = getTitle ma iMin iMax size reg1 reg2
                            return title
            L.getDiagram htmlFile iMin iMax step link title
            existsHTML <- L.doesNameExist htmlFile
            checkNewFile existsHTML htmlFile fileType mvb
            return ()
      
        -- routine for creating a csv file
        auxCSVs dat place1 place2 fileType mvb = do
            let L.MyWeatherProject{L.pathCSVGlobal=cg,L.pathCSVMilan=cm,L.pathCSV=cp,L.globalData=mapGlobal,L.milanData=mapMilan,L.avgData=mapAVG,L.region1=tvReg1,L.region2=tvReg2,L.pathMyCSV=mycsv} = dat
            cg' <- L.getTVar cg
            cm' <- L.getTVar cm
            cmy <- L.getTVar mycsv
                    
            let csvGlobalFile = L.setFilePath cp cg'
            let csvMilanFile = L.setFilePath cp cm'
            let csvMovingAverageFile = L.setFilePath cp cmy
                    
            existsCsvGlobal <- L.doesNameExist csvGlobalFile
            existsCsvMilan <- L.doesNameExist csvMilanFile
            checkFilePaths existsCsvGlobal existsCsvMilan place1 place2 mvb
            csvGlobal <- BL.readFile csvGlobalFile
            csvMilan <- BL.readFile csvMilanFile
            L.parsecsv' csvMilan mapMilan
            L.parsecsv' csvGlobal mapGlobal
            L.loopThroughData mapGlobal mapMilan mapAVG
            checkMaps mapMilan mapGlobal mapAVG tvReg1 tvReg2 mvb
            L.writeToCSV csvMovingAverageFile mapAVG tvReg1 tvReg2
            existsMACsv <- L.doesNameExist csvMovingAverageFile
            checkNewFile existsMACsv cmy fileType mvb
            return ()
        
        -- calling the necessary routines based on what the user wants
        checkArguments res dat = do
            let csvT = "csv"
            let htmlT = "html"
            case res of
                (L.TestData,mverbose) -> do
                    -- csv
                    auxCSVs dat "global temperatures" "temperatures for Milan" "csv" mverbose
                    
                    -- diagram
                    let L.MyWeatherProject{L.avgData=mapAVG} = dat
                    auxDiagram dat (L.TestData) htmlT mverbose mapAVG
                (L.UserData l,mverbose) -> do
                    -- csv
                    L.setArguments l dat
                    let L.MyWeatherProject{L.region1=tvReg1,L.region2=tvReg2} = dat
                    reg1 <- L.getTVar tvReg1
                    reg2 <- L.getTVar tvReg2
                    let temp1 = getTempTitle reg1
                    let temp2 = getTempTitle reg2
                    auxCSVs dat temp1 temp2 csvT mverbose
                (L.UserChart l,Just L.Verbose) -> do
                    -- diagram
                    auxUserDiagram dat (Just L.Verbose) l htmlT
                    return ()
                    
                (L.UserChart l,_) -> do
                    -- diagram
                    auxUserDiagram dat Nothing l htmlT
                    return ()
