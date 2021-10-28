start moves height tower1 tower2 tower3 = do 
          print tower1
          print tower2
          print tower3
          c <- getLine
          let com = words c
          if(head com /= "q") then
              if(head com == "b") then do -- TODO: resette listene ved opprettelse av nytt brett
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
                    drawRings [5] [5,3,2,1] [6,5]   height 1
                    --drawRings (ringPos tower1 height) tower2 tower3 height 1
                    goto(0, height + 2)
                    start 1 height (ringPos tower1 height) tower2 tower3
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

ringPos towerNr n = reverse [x |x <- [1..n]]                           

drawRings tower1 tower2 tower3 height spacing = if (odd height) then drawRingsOdd tower1 tower2 tower3 height spacing
                                                else drawRingsPar tower1 tower2 tower3 height spacing

drawRingsOdd [] [] [] height spacing = return ()
drawRingsOdd [] [] tower3 height spacing = do writeAt ((5*height- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                              drawRings [] [] (tail tower3) (height-1) spacing
drawRingsOdd [] tower2 [] height spacing = do writeAt ((3*height - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))
                                              drawRings [] (tail tower2) [] (height-1) spacing
drawRingsOdd tower1 [] [] height spacing = do writeAt ((height - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                              drawRings (tail tower1) [] [] (height-1) spacing
drawRingsOdd tower1 [] tower3 height spacing = do writeAt ((height - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                  writeAt ((5*height- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                                  drawRings (tail tower1) [] (tail tower3) (height-1) spacing
drawRingsOdd [] tower2 tower3 height spacing = do writeAt ((3*height - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))                                        
                                                  writeAt ((5*height- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                                  drawRings [] (tail tower2) (tail tower3) (height-1) spacing
drawRingsOdd tower1 tower2 [] height spacing = do writeAt ((height - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                  writeAt ((3*height - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))
                                                  drawRings (tail tower1) (tail tower2) [] (height-1) spacing
drawRingsOdd tower1 tower2 tower3 height spacing = do writeAt ((height - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                      writeAt ((3*height - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))                                        
                                                      writeAt ((5*height- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
                                                      drawRings (tail tower1) (tail tower2) (tail tower3) (height-1) spacing
drawRingsPar [] [] [] height spacing = return ()
drawRingsPar [] [] tower3 height spacing = undefined
drawRingsPar [] tower2 [] height spacing = undefined
drawRingsPar tower1 [] [] height spacing = undefined
drawRingsPar tower1 [] tower3 height spacing = undefined
drawRingsPar [] tower2 tower3 height spacing = undefined
drawRingsPar tower1 tower2 [] height spacing = undefined
drawRingsPar tower1 tower2 tower3 height spacing = do writeAt ((height - (head tower1) + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                      writeAt ((3*height - ((head tower2) - 1)), (height + 1)) (concat (take (head tower2) (repeat("# "))))                                        
                                                      writeAt ((5*height- ((head tower3) - 1)), height + 1) (concat (take (head tower3) (repeat("# "))))
    
    
                                                        --writeAt ((head tower1 + 1), (height + 1)) (concat (take (head tower1) (repeat("# "))))
                                                      --writeAt ((3*(head tower1) + 2), (height + 1)) (concat (take (head tower2) (repeat("# "))))                                        
                                                      --writeAt ((5*(head tower1)) + 5, (height + 1)) (concat (take (head tower3) (repeat("# "))))
                 
-- draws all the tower-layers
drawTowers height width = if height == 0 then return ()
                           else do writeRow height width
                                   drawTowers (height - 1) width

-- draws each single tower-layer
writeRow height width = putStrLn (concat (take 3 (repeat ((concat (take (width-1) (repeat " "))) ++ "|" ++ (concat (take width (repeat " ")))))))

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