import Text.Read
import Data.Maybe

start :: IO ()
start = do  c <- getLine
            let com = words c
            if(com == []) then start
            else if(head com == "q") then return ()
            else if(head com == "b") then do 
                let h = readMaybe (head (tail com)) :: Maybe Int
                if(isNothing h) then start
                else startGame (fromJust h)
            else do putStrLn "Unknown command" 
                    start

hanoi :: Int -> [([Int], [Int], [Int])] -> Int -> [Int] -> [Int] -> [Int] -> String -> IO () 
hanoi moves history height tower1 tower2 tower3 str =
    if(length tower3 == height) then winnerPrinter (moves-1) height
    else do
        statusPrinter moves height str
        c <- getLine
        let com = words c
        if(com == []) then hanoi moves history height tower1 tower2 tower3 ""
        else if(head com /= "q") then
            if(head com == "b") then do
                let h = readMaybe (head (tail com)) :: Maybe Int
                if(isNothing h) then hanoi moves history height tower1 tower2 tower3 str
                else startGame (fromJust h) 
            else if(head com == "z") then do
                let x = readMaybe (head (tail com)) :: Maybe Int
                if(isNothing x) then hanoi moves history height tower1 tower2 tower3 str
                else do
                    let n = fromJust x    
                    if((abs n) >= moves) then resetAllMoves height history str
                    else undoNMoves height history moves (abs n) str   
            else do
                let x = readMaybe (head com) :: Maybe Int
                    y = readMaybe (last com) :: Maybe Int
                if(isNothing x || isNothing y) then hanoi moves history height tower1 tower2 tower3 "Unvalid move"
                else do
                let f = fromJust x
                    t = fromJust y
                if ((f>0 && t<4) && (f>0 && t<4) && f /= t) then do
                    let currentMove = findCorrectTower f t tower1 tower2 tower3
                    if (legalMove currentMove) then do
                        let doneMove = doMove currentMove
                        nextRound moves history height f t doneMove tower1 tower2 tower3 str
                    else hanoi moves history height tower1 tower2 tower3 "Unvalid move"                        
                else hanoi moves history height tower1 tower2 tower3 "Unvalid move"     
        else return ()

startGame :: Int -> IO ()
startGame height = do  let moves = 0
                           tower1 = [] :: [Int]
                           tower2 = [] :: [Int]
                           tower3 = [] :: [Int]
                           history = [] :: [([Int],[Int],[Int])] 
                       if(height <= 1 || height > 12) then do
                            putStrLn "Unvalid nr of rings. The nr of rings needs to be between 2 and 12."
                            start
                       else do
                            drawBoard height
                            let startRings = ringPos tower1 height
                            drawRings startRings tower2 tower3 height height 
                            hanoi moves (addHistory history startRings tower2 tower3) height startRings tower2 tower3 ""

drawBoard :: Int -> IO ()
drawBoard height = do clr
                      goto(0,0)
                      drawTowers (height + 1) height
                      putStr "\ESC[0J"

undoNMoves :: Int -> [([Int], [Int], [Int])] -> Int -> Int -> String -> IO ()
undoNMoves height history moves n str = do goto(0,0) 
                                           drawTowers (height + 1) height
                                           let newHistory = (last (take (length history - n) history))
                                           drawRings (fst3 newHistory) (snd3 newHistory) (trd3 newHistory) height height
                                           hanoi (moves-n) [newHistory] height (fst3 newHistory) (snd3 newHistory) (trd3 newHistory) str

--lagrer ikke korrekt, resetter til 2. siste history
resetAllMoves :: Int -> [([Int], [Int], [Int])] -> String -> IO ()
resetAllMoves height history str = do drawBoard height
                                      drawRings (ringPos (head history) height) [] [] height height
                                      let moves = 0
                                      hanoi moves [head history] height (fst3 (head history)) [] [] str

fst3 :: ([Int], [Int], [Int]) -> [Int]
fst3 (x, _, _) = x
snd3 :: ([Int], [Int], [Int]) -> [Int]
snd3 (_, x, _) = x
trd3 :: ([Int], [Int], [Int]) -> [Int]
trd3 (_, _, x) = x

statusPrinter :: Show a => a -> Int -> String -> IO ()
statusPrinter moves height str = do goto(0, height + 2)
                                    putStrLn ("Number of moves: " ++ show moves)
                                    goto(30, height + 2)
                                    putStrLn str
                                    putStr "\ESC[0J"

winnerPrinter :: Show a => a -> Int -> IO ()
winnerPrinter moves height = do goto(0, height + 2)
                                putStrLn ("You successfully completed the game on level " ++ show height ++ " using " ++ show moves ++ " moves!")
                                main 

addHistory :: [([Int], [Int], [Int])] -> [Int] -> [Int] -> [Int] -> [([Int], [Int], [Int])]
addHistory history tower1 tower2 tower3 = history ++ [(tower1, tower2, tower3)]

ringPos :: (Num a, Enum a) => p -> a -> [a]
ringPos towerNr n = reverse [x |x <- [1..n]]    

legalMove :: ([Int], [Int]) -> Bool
legalMove ([], towerTo)        = False
legalMove (towerFrom, [])      = True
legalMove (towerFrom, towerTo) | (last towerFrom > last towerTo) = False
                               | otherwise                       = True

doMove :: ([Int], [Int]) -> ([Int], [Int])
doMove (towerFrom, towerTo) = ((init towerFrom), (towerTo ++ [last towerFrom]))

findCorrectTower :: (Eq a1, Eq a2, Num a1, Num a2) => a1 -> a2 -> [Int] -> [Int] -> [Int] -> ([Int], [Int])
findCorrectTower f t tower1 tower2 tower3 | (f==1 && t==2) = (tower1, tower2)
                                          | (f==1 && t==3) = (tower1, tower3)
                                          | (f==2 && t==3) = (tower2, tower3)
                                          | (f==2 && t==1) = (tower2, tower1)
                                          | (f==3 && t==1) = (tower3, tower1)
                                          | otherwise      = (tower3, tower2)

nextRoundHelper :: Int -> [([Int], [Int], [Int])] -> Int -> String -> [Int] -> [Int] -> [Int] -> IO ()
nextRoundHelper moves history height str tower1 tower2 tower3 = do  goto(0,0)
                                                                    drawTowers (height + 1) height
                                                                    drawRings tower1 tower2 tower3 height height
                                                                    goto(0,height+3)
                                                                    hanoi (moves+1) (addHistory history tower1 tower2 tower3) height tower1 tower2 tower3 str

nextRound :: (Eq a1, Eq a2, Num a1, Num a2) => Int -> [([Int], [Int], [Int])] -> Int -> a1 -> a2 -> ([Int], [Int]) -> [Int] -> [Int] -> [Int] -> String -> IO ()
nextRound moves history height f t (towerFrom, towerTo) tower1 tower2 tower3 str    | (f==1 && t==2) = nextRoundHelper moves history height str towerFrom towerTo tower3
                                                                                    | (f==1 && t==3) = nextRoundHelper moves history height str towerFrom tower2 towerTo
                                                                                    | (f==2 && t==3) = nextRoundHelper moves history height str tower1 towerFrom towerTo
                                                                                    | (f==2 && t==1) = nextRoundHelper moves history height str towerTo towerFrom tower3
                                                                                    | (f==3 && t==1) = nextRoundHelper moves history height str towerTo tower2 towerFrom
                                                                                    | otherwise      = nextRoundHelper moves history height str tower1 towerTo towerFrom                                               

drawRings :: [Int] -> [Int] -> [Int] -> Int -> Int -> IO ()
drawRings tower1 tower2 tower3 height width = do drawRingLayer tower1 tower2 tower3 height width
                                                 goto(0, height + 2)

-- refaktorer?
drawRingLayer :: [Int] -> [Int] -> [Int] -> Int -> Int -> IO ()
drawRingLayer [] [] [] height width             = return ()
drawRingLayer [] [] tower3 height width         = do drawRingLayerHelper 5 width height tower3
                                                     drawRings [] [] (tail tower3) (height-1) width
drawRingLayer [] tower2 [] height width         = do drawRingLayerHelper 3 width height tower2
                                                     drawRings [] (tail tower2) [] (height-1) width
drawRingLayer tower1 [] [] height width         = do drawRingLayerHelper 1 width height tower1
                                                     drawRings (tail tower1) [] [] (height-1) width
drawRingLayer tower1 [] tower3 height width     = do drawRingLayerHelper 1 width height tower1
                                                     drawRingLayerHelper 5 width height tower3
                                                     drawRings (tail tower1) [] (tail tower3) (height-1) width
drawRingLayer [] tower2 tower3 height width     = do drawRingLayerHelper 3 width height tower2
                                                     drawRingLayerHelper 5 width height tower3
                                                     drawRings [] (tail tower2) (tail tower3) (height-1) width
drawRingLayer tower1 tower2 [] height width     = do drawRingLayerHelper 1 width height tower1
                                                     drawRingLayerHelper 3 width height tower2
                                                     drawRings (tail tower1) (tail tower2) [] (height-1) width
drawRingLayer tower1 tower2 tower3 height width = do drawRingLayerHelper 1 width height tower1
                                                     drawRingLayerHelper 3 width height tower2
                                                     drawRingLayerHelper 5 width height tower3
                                                     drawRings (tail tower1) (tail tower2) (tail tower3) (height-1) width

drawRingLayerHelper :: Int -> Int -> Int -> [Int] -> IO ()
drawRingLayerHelper n width height towerNr = writeAt ((n*width- ((head towerNr) - 1)), height + 1) (concat (take (head towerNr) (repeat("# "))))
                            
-- draws all the tower-layers
drawTowers :: (Eq t, Num t) => t -> Int -> IO ()
drawTowers 0 _ = return ()
drawTowers height width = do writeTowerLayer height width
                             drawTowers (height - 1) width

-- draws each single tower-layer
writeTowerLayer :: p -> Int -> IO ()
writeTowerLayer height width = putStrLn (concat (take 3 (repeat ((concat (take (width-1) (repeat " "))) ++ "|" ++ (concat (take width (repeat " ")))))))

writeAt (x,y) str = do goto (x,y)
                       putStr str

--clears the terminal-window
clr :: IO ()
clr = putStr "\ESC[2J"

-- changes pos of marker
goto :: (Int, Int) -> IO ()
goto(x,y) = putStr("\ESC["++ show y ++";" ++ show x++"H")

main :: IO ()
main = do putStrLn "Start a new game with: b <nbOfRings>, or quit with: q "
          start

