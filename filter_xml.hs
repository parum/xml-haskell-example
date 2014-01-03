import System.IO
import Text.XML.Light
import Data.List
import Text.CSV (printCSV)

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
      
main :: IO ()
main = do
  infile <- openFile "dacs/dolbyAtmosConfiguration.xml" ReadMode
  doc <- loadXml infile
  let roomField = elementValuesFromNames doc roomDimensionParams
--  putStrLn $ printCSV (roomDimensionParams : roomField : [])
  let docNames = speakerNames doc
  --print docNames
  let numSpeakersPerType = map length $ map (filterNamesWithPrefixes docNames) (map snd speakerTypes) 
  putStrLn $ printCSV $ 
    -- column names
    (roomDimensionParams ++ (map ((++" count").fst) speakerTypes)) : 
    -- data
    (roomField ++ (map show numSpeakersPerType)) 
    : []
  hClose infile

loadXml :: Handle -> IO Element
loadXml file = do
  xml <- hGetContents file
  let result = case (parseXMLDoc xml) of 
	      Just doc -> doc
	      Nothing -> error "Bad xml doc"
  return result

 -- Pure:
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