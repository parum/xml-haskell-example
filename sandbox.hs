import System.IO
import System.Directory
import Data.List
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L
import Text.XML.Light
import Text.CSV (printCSV)

-- do with file

--dacFiles = ["dacs/a.dac", "dacs/b.dac"]

main :: IO ()
main = do
  allFiles <- getDirectoryContents "dacs"
  let dacFiles = map ("dacs/"++) $ filter (isSuffixOf ".dac") allFiles
  print dacFiles
  sequence_ $ map (doWithZipFile takeSome) dacFiles


doWithZipFile :: (L.ByteString -> L.ByteString) -> String -> IO ()
doWithZipFile f filename = do
  content <- unzipFile filename
  print $ f content

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
takeSomeS s = take 100 s

takeSome :: L.ByteString -> L.ByteString
takeSome s = L.take 100 s

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
