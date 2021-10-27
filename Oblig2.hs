start moves height tower1 tower2 tower3 = do 
          print tower1
          print tower2
          print tower3
          c <- getLine
          let com = words c
          if(head com /= "q") then
              if(head com == "b") then do
                  clr
                  let moves = 0 
                  let height = read (head (tail com)) :: Int
                  if(height <= 1 || height > 12) then do
                      print "Unvalid nr of rings. The nr of rings needs to be between 2 and 12."
                      start 0 1 tower1 tower2 tower3
                  else do
                    goto(0,0)
                    drawTowers (height + 1) height
                    putStr "\ESC[0J"
                    drawRings (saveRingPos tower1 height) tower2 tower3 height height 1
                    goto(0, height + 2)
                    start 1 height (saveRingPos tower1 height) tower2 tower3
              else do
                let f = read (head com) :: Int
                    t = read (last com) :: Int
                if ((f>0 && f<4) && (t>0 && t<4)) then do
                    let currentMove = findCorrectTower f t tower1 tower2 tower3
                    if (legalMove currentMove) then do
                        let doneMove = doMove currentMove
                        statusPrinter moves height "             "
                        nextRound moves height f t doneMove tower1 tower2 tower3
                    else do statusPrinter moves height "Unvalid move"
                            start moves height tower1 tower2 tower3                        
                else do statusPrinter moves height "Unvalid move"
                        start moves height tower1 tower2 tower3      
          else return ()

-- liste med breddePåRing -> [6,5,4,3] [2,1] [] -> spacing = towerHeight - ringSize

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

nextRound moves height f t (towerFrom, towerTo) tower1 tower2 tower3 | (f==1 && t==2) = start (moves+1) height towerFrom towerTo tower3               
                                                                     | (f==1 && t==3) = start (moves+1) height towerFrom tower2 towerTo
                                                                     | (f==2 && t==3) = start (moves+1) height tower1 towerFrom towerTo
                                                                     | (f==2 && t==1) = start (moves+1) height towerTo towerFrom tower3
                                                                     | (f==3 && t==1) = start (moves+1) height towerTo tower2 towerFrom
                                                                     | otherwise      = start (moves+1) height tower1 towerTo towerFrom                          

statusPrinter moves height str = do goto(0, height + 2)
                                    putStrLn ("Number of moves: " ++ show moves)
                                    goto(30, height + 2)
                                    putStrLn str
                                    putStr "\ESC[0J"

saveRingPos towerNr width = if (width == 0) then towerNr
                            else saveRingPos (towerNr ++ [width]) (width - 1)

removeRingPos = undefined

--removeRing height width = writeAt (1, (height-4)) (concat (take 3 (repeat ((concat (take (width `div` 2) (repeat "  "))) ++ "|" ++ (concat (take (width `div` 2) (repeat "  ")))))))

{-
drawRings height width spacing = if (height == 0) then return ()
                                 else do drawRing height width spacing
                                         drawRings (height-1) width (spacing + 1)
-}

drawRings tower1 tower2 tower3 towerHeight ringHeight spacing = if(length tower1 > 0) then do drawRing (head tower1) ringHeight spacing
                                                                                              drawRings (tail tower1) tower2 tower3 towerHeight (ringHeight - 1) (spacing + 1)
                                                                else do 
                                                                    if(length tower2 > 0) then do let ringHeight = towerHeight
                                                                                                  drawRing (head tower1) towerHeight (spacing + towerHeight)
                                                                                                  drawRings (tail tower1) tower2 tower3 towerHeight (ringHeight - 1) (spacing + 1)
                                                                    else return ()

drawRing tower1 tower2 tower3 height spacing = writeAt (0, (height+1)) ((concat (take spacing (repeat " "))) ++ (concat (take height (repeat("# ")))))

--drawRing width height spacing = writeAt (0, (height+1)) ((concat (take spacing (repeat " "))) ++ (concat (take height (repeat("# ")))))      
                  
-- draws all the tower-layers
drawTowers height width = if height == 0 then return ()
                           else do writeRow height width
                                   drawTowers (height - 1) width

-- draws each single tower-layer
writeRow height width = putStrLn (concat (take 3 (repeat ((concat (take (width `div` 2) (repeat "  "))) ++ "|" ++ (concat (take (width `div` 2) (repeat "  ")))))))

writeAt (x,y) str = do goto (x,y)
                       putStr str

--clears the terminal-window
clr = putStr "\ESC[2J"

-- changes pos of marker
goto :: (Int, Int) -> IO ()
goto(x,y) = putStr("\ESC["++ show y ++";" ++ show x++"H")

main :: IO ()
main = do putStrLn "Start a new game with: b <nbOfRings>, or quit with: q "
          start 0 1 ([] :: [Int]) ([] :: [Int]) ([] :: [Int]) 