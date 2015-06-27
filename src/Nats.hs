module Nats (main) where


import Data.List
import System.Environment
import System.Exit


nats :: [Integer]
nats = [0 ..]


tryRead :: Read a => String -> Maybe a
tryRead str = case reads str of
    [(x, "")] -> Just x
    _ -> Nothing


tryReadNat :: String -> Maybe Integer
tryReadNat str = case tryRead str of
    Nothing -> Nothing
    Just n -> if n < 0
        then Nothing
        else Just n


main :: IO ()
main = do
    args <- getArgs
    ns <- case args of
        [] -> return nats
        [arg] -> case tryReadNat arg of
            Nothing -> printHelp >> exitFailure
            Just begin -> return $ genericDrop begin nats
        [arg0, arg1] -> case mapM tryReadNat [arg0, arg1] of
            Nothing -> printHelp >> exitFailure
            Just [begin, end] -> return $ takeWhile (/= end + 1) $ genericDrop begin nats
    mapM_ print ns


printHelp :: IO ()
printHelp = do
    putStrLn "nats [BEGIN [END]]"
    putStrLn "Streams the natural numbers [0,) line by line forever. If BEGIN or END are supplied, then print only from [BEGIN, END]"




