import Text.Read
import Data.Maybe

start :: IO ()
start = do
    c <- getLine
    let com = words c
    if(com == []) then start
    else if(head com == "q") then return ()
    else if(head com == "b") then startGame com
    else do putStrLn "Unknown command" 
            start
    

hanoi moves history height tower1 tower2 tower3 str = do  -- game-loop
    if(length tower3 == height) then winnerPrinter (moves-1) height
    else do
        statusPrinter moves height str
        c <- getLine
        let com = words c
        if(com == []) then hanoi moves history height tower1 tower2 tower3 ""
        else if(head com /= "q") then
            if(head com == "b") then do
                startGame com 
            else if(head com == "z") then do
                let n = read (head (tail com)) :: Int
                if((abs n) >= moves) then
                    resetAllMoves height history str
                else do
                    undoNMoves height history moves (abs n) str   
            else do
                let x = readMaybe (head com) :: Maybe Int
                    y = readMaybe (last com) :: Maybe Int
                if(isNothing x && isNothing y) then 
                    hanoi moves history height tower1 tower2 tower3 "Unvalid move"
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

startGame com = do let height = read (head (tail com)) :: Int
                       moves = 0
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

undoNMoves height history moves n str = do goto(0,0) 
                                           drawTowers (height + 1) height
                                           let newHistory = (last (take (length history - n) history))
                                           drawRings (fst3 newHistory) (snd3 newHistory) (trd3 newHistory) height height
                                           hanoi (moves-n) [newHistory] height (fst3 newHistory) (snd3 newHistory) (trd3 newHistory) str

--lagrer ikke korrekt, resetter til 2. siste history
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

statusPrinter moves height str = do goto(0, height + 2)
                                    putStrLn ("Number of moves: " ++ show moves)
                                    goto(30, height + 2)
                                    putStrLn str
                                    putStr "\ESC[0J"

winnerPrinter moves height = do goto(0, height + 2)
                                putStrLn ("You successfully completed the game on level " ++ show height ++ " using " ++ show moves ++ " moves!")
                                main 

addHistory history tower1 tower2 tower3 = history ++ [(tower1, tower2, tower3)]

ringPos towerNr n = reverse [x |x <- [1..n]]    

legalMove ([], towerTo)        = False
legalMove (towerFrom, [])      = True
legalMove (towerFrom, towerTo) | (last towerFrom > last towerTo) = False
                               | otherwise                       = True
                         
doMove (towerFrom, towerTo) = ((init towerFrom), (towerTo ++ [last towerFrom]))

findCorrectTower f t tower1 tower2 tower3 | (f==1 && t==2) = (tower1, tower2)
                                          | (f==1 && t==3) = (tower1, tower3)
                                          | (f==2 && t==3) = (tower2, tower3)
                                          | (f==2 && t==1) = (tower2, tower1)
                                          | (f==3 && t==1) = (tower3, tower1)
                                          | otherwise      = (tower3, tower2)

-- TODO: Refaktorer denne og
nextRound moves history height f t (towerFrom, towerTo) tower1 tower2 tower3 str    | (f==1 && t==2) = do goto(0,0)
                                                                                                          drawTowers (height + 1) height
                                                                                                          drawRings towerFrom towerTo tower3 height height
                                                                                                          goto(0,height+3)
                                                                                                          hanoi (moves+1) (addHistory history towerFrom towerTo tower3) height towerFrom towerTo tower3 str              
                                                                                    | (f==1 && t==3) = do goto(0,0)
                                                                                                          drawTowers (height + 1) height
                                                                                                          drawRings towerFrom tower2 towerTo height height
                                                                                                          goto(0,height+3)
                                                                                                          hanoi (moves+1) (addHistory history towerFrom tower2 towerTo) height towerFrom tower2 towerTo str
                                                                                    | (f==2 && t==3) = do goto(0,0)
                                                                                                          drawTowers (height + 1) height
                                                                                                          drawRings tower1 towerFrom towerTo height height
                                                                                                          goto(0,height+3)
                                                                                                          hanoi (moves+1) (addHistory history tower1 towerFrom towerTo) height tower1 towerFrom towerTo str
                                                                                    | (f==2 && t==1) = do goto(0,0)
                                                                                                          drawTowers (height + 1) height
                                                                                                          drawRings towerTo towerFrom tower3 height height
                                                                                                          goto(0,height+3)
                                                                                                          hanoi (moves+1) (addHistory history towerTo towerFrom tower3) height towerTo towerFrom tower3 str
                                                                                    | (f==3 && t==1) = do goto(0,0)
                                                                                                          drawTowers (height + 1) height
                                                                                                          drawRings towerTo tower2 towerFrom height height
                                                                                                          goto(0,height+3)
                                                                                                          hanoi (moves+1) (addHistory history towerTo tower2 towerFrom) height towerTo tower2 towerFrom str
                                                                                    | otherwise      = do goto(0,0)
                                                                                                          drawTowers (height + 1) height
                                                                                                          drawRings tower1 towerTo towerFrom height height
                                                                                                          goto(0,height+3)
                                                                                                          hanoi (moves+1) (addHistory history tower1 towerTo towerFrom) height tower1 towerTo towerFrom str                                               

drawRings tower1 tower2 tower3 height width = do drawRingLayer tower1 tower2 tower3 height width
                                                 goto(0, height + 2)

-- TODO: Refaktorer denne metoden!
drawRingLayer [] [] [] height width = return ()
drawRingLayer [] [] tower3 height width = do writeAt ((5*width- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                             drawRings [] [] (tail tower3) (height-1) width
drawRingLayer [] tower2 [] height width = do writeAt ((3*width - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))
                                             drawRings [] (tail tower2) [] (height-1) width
drawRingLayer tower1 [] [] height width = do writeAt ((width - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                             drawRings (tail tower1) [] [] (height-1) width
drawRingLayer tower1 [] tower3 height width = do writeAt ((width - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                 writeAt ((5*width- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                                 drawRings (tail tower1) [] (tail tower3) (height-1) width
drawRingLayer [] tower2 tower3 height width = do writeAt ((3*width - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))                                        
                                                 writeAt ((5*width- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                                 drawRings [] (tail tower2) (tail tower3) (height-1) width
drawRingLayer tower1 tower2 [] height width = do writeAt ((width - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                 writeAt ((3*width - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))
                                                 drawRings (tail tower1) (tail tower2) [] (height-1) width
drawRingLayer tower1 tower2 tower3 height width = do writeAt ((height - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                     writeAt ((3*width - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))                                        
                                                     writeAt ((5*width- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                                     drawRings (tail tower1) (tail tower2) (tail tower3) (height-1) width
                            
-- draws all the tower-layers
drawTowers 0 _ = return ()
drawTowers height width = do writeTowerLayer height width
                             drawTowers (height - 1) width

-- draws each single tower-layer
writeTowerLayer height width = putStrLn (concat (take 3 (repeat ((concat (take (width-1) (repeat " "))) ++ "|" ++ (concat (take width (repeat " ")))))))

writeAt (x,y) str = do goto (x,y)
                       putStr str

--clears the terminal-window
clr = putStr "\ESC[2J"

-- changes pos of marker
goto :: (Int, Int) -> IO ()
goto(x,y) = putStr("\ESC["++ show y ++";" ++ show x++"H")

main :: IO ()
main = do putStrLn "Start a new game with: b <nbOfRings>, or quit with: q "
          start
