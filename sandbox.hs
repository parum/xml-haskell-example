import System.IO
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L

-- do with file

dacFiles = ["dacs/a.dac", "dacs/b.dac"]

main :: IO ()
main = do
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



takeSome :: L.ByteString -> L.ByteString
takeSome s = L.take 100 s
