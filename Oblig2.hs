
start moves history height tower1 tower2 tower3 = 
    if(length tower3 == height) then winnerPrinter (moves-1) height
    else do
        c <- getLine
        let com = words c
        if(head com /= "q") then
            if(head com == "b") then do
                clr
                -- reset all the tower-lists when starting a new game
                let tower1 = [] :: [Int]
                    tower2 = [] :: [Int]
                    tower3 = [] :: [Int]
                    history = [] :: [([Int],[Int],[Int])]
                let moves = 0 
                let height = read (head (tail com)) :: Int
                if(height <= 1 || height > 12) then do
                    print "Unvalid nr of rings. The nr of rings needs to be between 2 and 12."
                    start 0 history 1 tower1 tower2 tower3
                else do
                    goto(0,0)
                    drawTowers (height + 1) height
                    putStr "\ESC[0J"
                    let startRings = ringPos tower1 height
                    drawRings startRings tower2 tower3 height height
                    goto(0, height + 2)
                    start 1 (addHistory history startRings tower2 tower3) height startRings tower2 tower3
            else if(head com == "z") then do 
                    let n = read (head (tail com)) :: Int
                    if(n >= moves) then do
                        goto(0,0) 
                        drawTowers (height + 1) height
                        drawRings (ringPos (head history) height) [] [] height height
                        goto(0, height + 2)
                        let moves = 0
                        putStrLn ("Number of moves: " ++ show moves)
                        start 0 [head history] height (fst3 (head history)) [] []
                    else do 
                        goto(0,0) 
                        drawTowers (height + 1) height
                        drawRings (fst3 (last (take (length history - n) history))) (snd3 (last (take (length history - n) history))) (trd3 (last (take (length history - n) history))) height height
                        goto(0, height + 2)
                        putStrLn ("Number of moves: " ++ show (moves-n-1))
                        start (moves-n) [(last (take (length history - n) history))] height (fst3 (last (take (length history - n) history))) (snd3 (last (take (length history - n) history))) (trd3 (last (take (length history - n) history)))
            else do
                let f = read (head com) :: Int
                    t = read (last com) :: Int
                if ((f>0 && f<4) && (t>0 && t<4) && f /= t) then do
                    let currentMove = findCorrectTower f t tower1 tower2 tower3
                    if (legalMove currentMove) then do
                        let doneMove = doMove currentMove
                        statusPrinter moves height "             "
                        nextRound moves history height f t doneMove tower1 tower2 tower3
                    else do statusPrinter (max 0 (moves-1)) height "Unvalid move"
                            start moves history height tower1 tower2 tower3                        
                else do statusPrinter (max 0 (moves-1)) height "Unvalid move"
                        start moves history height tower1 tower2 tower3      
        else return ()

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

legalMove ([], towerTo) = False
legalMove (towerFrom, []) = True
legalMove (towerFrom, towerTo) | (last towerFrom > last towerTo) = False
                               | otherwise = True
                         
doMove (towerFrom, towerTo) = ((init towerFrom), (towerTo ++ [last towerFrom]))

findCorrectTower f t tower1 tower2 tower3 | (f==1 && t==2) = (tower1, tower2)
                                          | (f==1 && t==3) = (tower1, tower3)
                                          | (f==2 && t==3) = (tower2, tower3)
                                          | (f==2 && t==1) = (tower2, tower1)
                                          | (f==3 && t==1) = (tower3, tower1)
                                          | otherwise      = (tower3, tower2)

nextRound moves history height f t (towerFrom, towerTo) tower1 tower2 tower3 | (f==1 && t==2) = do goto(0,0)
                                                                                                   drawTowers (height + 1) height
                                                                                                   drawRings towerFrom towerTo tower3 height height
                                                                                                   goto(0,height+3)
                                                                                                   start (moves+1) (addHistory history towerFrom towerTo tower3) height towerFrom towerTo tower3               
                                                                             | (f==1 && t==3) = do goto(0,0)
                                                                                                   drawTowers (height + 1) height
                                                                                                   drawRings towerFrom tower2 towerTo height height
                                                                                                   goto(0,height+3)
                                                                                                   start (moves+1) (addHistory history towerFrom tower2 towerTo) height towerFrom tower2 towerTo
                                                                             | (f==2 && t==3) = do goto(0,0)
                                                                                                   drawTowers (height + 1) height
                                                                                                   drawRings tower1 towerFrom towerTo height height
                                                                                                   goto(0,height+3)
                                                                                                   start (moves+1) (addHistory history tower1 towerFrom towerTo) height tower1 towerFrom towerTo
                                                                             | (f==2 && t==1) = do goto(0,0)
                                                                                                   drawTowers (height + 1) height
                                                                                                   drawRings towerTo towerFrom tower3 height height
                                                                                                   goto(0,height+3)
                                                                                                   start (moves+1) (addHistory history towerTo towerFrom tower3) height towerTo towerFrom tower3
                                                                             | (f==3 && t==1) = do goto(0,0)
                                                                                                   drawTowers (height + 1) height
                                                                                                   drawRings towerTo tower2 towerFrom height height
                                                                                                   goto(0,height+3)
                                                                                                   start (moves+1) (addHistory history towerTo tower2 towerFrom) height towerTo tower2 towerFrom
                                                                             | otherwise      = do goto(0,0)
                                                                                                   drawTowers (height + 1) height
                                                                                                   drawRings tower1 towerTo towerFrom height height
                                                                                                   goto(0,height+3)
                                                                                                   start (moves+1) (addHistory history tower1 towerTo towerFrom) height tower1 towerTo towerFrom                                                 

drawRings tower1 tower2 tower3 height width = drawRingLayer tower1 tower2 tower3 height width

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
drawTowers height width = if height == 0 then return ()
                          else do writeTowerLayer height width
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
          start 0 [] 1 [] [] []
