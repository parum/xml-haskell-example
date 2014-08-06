import System.IO
import System.Directory
import Data.List
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.XML.Light
import Text.CSV (printCSV)

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

process :: String -> String
process = filterXmlS
--process = takeSomeS

main :: IO ()
main = do
  allFiles <- getDirectoryContents "."
  let dacFiles = filter (isSuffixOf ".dac") allFiles
  putStrLn "Parsed dac files:"
  print dacFiles
  putStrLn "\n\n"
  printHeader
  sequence_ $ map (doWithZipFileS $ process) dacFiles

printHeader = do
  putStrLn $ printCSV $ (infoParams ++ roomDimensionParams ++ (map ((++" count") . fst) speakerTypes)) : []

doWithZipFile :: (L.ByteString -> L.ByteString) -> String -> IO ()
doWithZipFile f filename = do
  content <- unzipFile filename
  print $ f content

doWithZipFileS :: (String -> String) -> String -> IO ()
doWithZipFileS f filename = do
  content <- unzipFile filename
  putStrLn $ f $ BS.unpack content

doWithFile :: (String -> String) -> String -> IO ()
doWithFile f filename = do
  file <- openFile filename ReadMode
  hGetContents file >>= putStrLn . f
  putStrLn ""
  hClose file

unzipFile :: String -> IO L.ByteString
unzipFile filename = do
  file <- L.readFile filename
  return $ fromEntry . head . zEntries . toArchive $ file


takeSomeS :: String -> String
takeSomeS s = filterValid $ take 1000 s

filterValid :: String -> String
filterValid s = filter validChar s
	where validChar c = c /= '*'


takeSome :: L.ByteString -> L.ByteString
takeSome s = L.take 10000 s

 -- Pure:
filterXmlS :: String -> String
filterXmlS content = 
	let doc = loadXmlFromString content
	    roomData = elementValuesFromNames doc roomDimensionParams
  	    spkNames = speakerNames doc
  	    numSpeakersPerType = map length $ map (filterNamesWithPrefixes spkNames) (map snd speakerTypes) 
  	    infoParamsData = map (lookupInfoParam doc) infoParams
  	in printCSV $ (infoParamsData ++ roomData ++ (map show numSpeakersPerType)) : []


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
  elementValueFromName name = strContent (head values) -- take the first result if repeated. TODO check that repeated are the same value
    where
    values = filterElementsName (hasName name) doc
    
hasName :: String -> QName -> Bool
hasName param (QName name _ _) 
      | name == param = True
      | otherwise = False
