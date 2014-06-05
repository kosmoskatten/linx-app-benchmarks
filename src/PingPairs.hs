module Main where

import System.Environment (getArgs)

-- | The PingPairs program is establishing N number of "ping pairs".
main :: IO ()
main = do
  args <- getArgs
  case args of
    [gateway, gatewayPort, pairs, frequencyTable] ->
      runPingPairs gateway (read gatewayPort) 
                           (read pairs) 
                           (read frequencyTable)
    _                                             -> help
      

runPingPairs :: String -> Int -> Int -> [(Int, Int)] -> IO ()
runPingPairs gw gwPort num freqs = do
  let freqs' = expandFrequencies freqs
  print gw
  print gwPort
  print num
  print freqs'

expandFrequencies :: [(Int, Int)] -> [Int]
expandFrequencies = concat . map (uncurry replicate)

help :: IO ()
help = do
  putStrLn "Usage: PingPairs <gateway address> <gateway port> <pairs> <freqs>"
  putStrLn " <gateway address>: IP address to gateway. E.g. 192.168.122.8"
  putStrLn " <gateway port>   : Port to gateway. E.g. 21768"
  putStrLn " <pairs>          : Communicating pairs. E.g. 7"
  putStrLn " <freqs>          : Ping intensity. E.g. \"[(10, 1000), (100, 100)]\""