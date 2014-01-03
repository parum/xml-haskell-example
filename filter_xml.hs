import System.IO
import Text.XML.Light
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

main :: IO ()
main = do
  --processXml "dad.xml"
  infile <- openFile "dad.xml" ReadMode
  doc <- loadXml infile
  let roomField = elementValuesFromNames doc roomDimensionParams
  putStrLn $ printCSV (roomDimensionParams : roomField : [])
  print $ speakerNames doc
  hClose infile

  
loadXml :: Handle -> IO Element
loadXml file = do
  xml <- hGetContents file
  let result = case (parseXMLDoc xml) of 
	      Just doc -> doc
	      Nothing -> error "Bad xml doc"
  return result

 -- Pure:

speakerNames :: Element -> [Element]
speakerNames doc = filterElementsName (filterInfo "name") doc
 
elementValuesFromNames :: Element -> [String] -> [String]
elementValuesFromNames doc names = map elementValueFromName names
  where
  elementValueFromName name = strContent (values !! 0) -- take the first result if repeated. TODO check that repeated are the same value
    where
    values = filterElementsName (filterInfo name) doc
    
filterInfo :: String -> QName -> Bool
filterInfo param (QName name _ _) 
      | name == param = True
      | otherwise = False