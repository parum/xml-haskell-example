import System.IO
import Text.XML.Light
import Data.List
import Text.CSV (printCSV)
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L

{-
Filter an xml file and export a CSV with
·         Room dimensions
·         Number of speakers per wall
·         Number of bass management subs
·         3 or 5 screen speakers (if possible)
·         Any other poignant room size settings
-}
roomDimensionParams = [
      "screenWallWidth",
      "boothWallWidth",
      "houseLeftWallWidth",
      "houseRightWallWidth",
      "floorElevationAtScreen",
      "ceilingElevationAtScreen"
      ]
speakerTypes = [
      ("screen speakers", ["L ", "C ", "R ", "Lc ", "Rc "]),
      ("subwoofers", ["Subwoofer"]),
      ("left side surrounds (Lw+Lss)", ["Lw", "Lss"]),
      ("left top surrounds", ["Lts"])
      ]
infoParams = ["company", "theaterName", "auditorium", "city", "state", "country"] 
      
main :: IO ()
main = do
--  print . L.take 100 =<< unzipFiles
  filterXml


unzipFiles :: IO L.ByteString
unzipFiles = do
  file <- L.readFile "dacs/foo.dac"
  return $ fromEntry . head . zEntries . toArchive $ file


filterXml :: IO ()
filterXml = do
  infile <- openFile "dacs/dolbyAtmosConfiguration.xml" ReadMode
  doc <- loadXmlFromFile infile
  let roomData = elementValuesFromNames doc roomDimensionParams
--  putStrLn $ printCSV (roomDimensionParams : roomData : [])
  let spkNames = speakerNames doc
  --print docNames
  let numSpeakersPerType = map length $ map (filterNamesWithPrefixes spkNames) (map snd speakerTypes) 
  let infoParamsData = map (lookupInfoParam doc) infoParams
  putStrLn $ printCSV $ 
    -- column names
    (infoParams     ++ roomDimensionParams ++ (map ((++" count").fst) speakerTypes)) : 
    -- data
    (infoParamsData ++ roomData            ++ (map show numSpeakersPerType)) 
    : []
  hClose infile
  

loadXmlFromFile :: Handle -> IO Element
loadXmlFromFile file = do
  xml <- hGetContents file
  return $ loadXmlFromString xml

 -- Pure:

loadXmlFromString :: String -> Element
loadXmlFromString xmlStr = 
  case (parseXMLDoc xmlStr) of 
       Just doc -> doc
       Nothing -> error "Bad xml doc"



lookupInfoParam :: Element -> String -> String
lookupInfoParam doc param = concat $ map strContent $ filterElementsName (hasName param) doc

filterNamesWithPrefixes :: [String] -> [String] -> [String]
filterNamesWithPrefixes names prefixes = filter (containsPrefix prefixes) names
  where
    containsPrefix :: [String] ->  String -> Bool
    containsPrefix prefixes name = any (`isPrefixOf` name) prefixes
  
speakerNames :: Element -> [String]
speakerNames doc = map strContent speakerNameElems
  where
    speakerNameElems = concat $ map (filterElementsName (hasName "name")) speakerElems
    speakerElems = filterElementsName (hasName "speakerEndpoint") doc
 
elementValuesFromNames :: Element -> [String] -> [String]
elementValuesFromNames doc names = map elementValueFromName names
  where
  elementValueFromName name = strContent (values !! 0) -- take the first result if repeated. TODO check that repeated are the same value
    where
    values = filterElementsName (hasName name) doc
    
hasName :: String -> QName -> Bool
hasName param (QName name _ _) 
      | name == param = True
      | otherwise = False