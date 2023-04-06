module Main where 
import qualified ParseInput as PI
import qualified DirectedGraph as DG
import Control.Monad (replicateM)

main :: IO ()
main = 
    do
        firstLine           <- getLine 
        let nS = PI.split $ PI.trim' firstLine
        let n1              = readInt (fst nS)
        let n2              = readInt (snd nS)
        words <- listInput n1  -- Fishy 
        let edges           = PI.createEdges (PI.subsetTuple words)
        let graph           = DG.createGraph edges
        queries             <- listInput n2 -- also Fishy
        let readyQueries    = map PI.split queries
        let answers         = runAlg graph readyQueries
        putStr (unlines answers)

--main :: IO () -- test main
--main = 
--    do
--        let firstLine = "10 10"
--        let nS = PI.split $ PI.trim' firstLine
--        let n1              = readInt (fst nS)
--        let n2              = readInt (snd nS)
--        let words           = ["rebkx","lalgu","tftzx","gteok","umjhr","uolcp","krlwe","zfcpz","dukgy","zfkvu"]
--        let edges           = PI.createEdges $ PI.subsetTuple words
--        let graph           = DG.createGraph edges
--        let queries         = ["umjhr dukgy","gteok uolcp","krlwe tftzx","lalgu umjhr","zfcpz rebkx","dukgy dukgy","tftzx rebkx","zfcpz lalgu","krlwe umjhr","dukgy krlwe"]
--        let readyQueries    = map PI.split queries
--        let answers         = runAlg graph readyQueries
--        putStr (unlines answers)

        

runAlg :: [DG.Node] -> [(DG.Vertex, DG.Vertex)] -> [String]
runAlg graph = map $ answer . uncurry (DG.bfs graph)

listInput :: Int -> IO [String] -- Gotta find a better way or smth 
listInput n = replicateM n getLine

answer :: Int -> String
answer input
    |input /= (maxBound :: Int) = show input
    |otherwise                  = "Impossible"
    

isInt :: String -> Bool
isInt = all $ flip elem $ concatMap show [0..9]

readInt :: String -> Int
readInt x = read x::Int