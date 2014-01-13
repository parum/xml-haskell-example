import System.IO

main :: IO ()
main = do
  outh <- openFile "output.txt" WriteMode
  consumeInputProcessAndAppend "input.txt" outh id
  consumeInputProcessAndAppend "input.txt" outh id
  hClose outh
  
-- Open process input file and append to output
consumeInputProcessAndAppend :: FilePath -> Handle -> (String -> String) -> IO ()
consumeInputProcessAndAppend inpath outh f = do
  inh <- openFile inpath ReadMode
  inputStr <- hGetContents inh
  hPutStr outh (f inputStr)
  hClose inh
    