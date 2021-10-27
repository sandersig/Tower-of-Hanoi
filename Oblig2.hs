start moves height tower1 tower2 tower3 = do 
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
                    drawRings height 0 1
                    goto(0, height + 2)
                    start 1 height (saveRingPos tower1 height) tower2 tower3
              else do
                let f = read (head com) :: Int
                    t = read (last com) :: Int
                    str = ("Number of moves: " ++ show moves)

                if (f==1 && t==2) then doMoveHelper (doMove tower1 tower2)
                else if (f==1 && t==3) then doMoveHelper (doMove tower1 tower3)
                else if (f==2 && t==3) then doMoveHelper (doMove tower2 tower3)
                else if (f==2 && t==1) then doMoveHelper (doMove tower2 tower1)
                else if (f==3 && t==2) then doMoveHelper (doMove tower3 tower2)
                else doMoveHelper (doMove tower3 tower1) 
                goto(0, height + 2)
                putStrLn str
                start (moves + 1) height tower1 tower2 tower3     
          else return ()

-- liste med breddePåRing -> [6,5,4,3,2,1]

doMoveHelper towerList = print towerList
                          
doMove towerFrom towerTo = (towerFrom ++ [last towerTo]) 
                           

saveRingPos towerNr width = if (width == 0) then towerNr
                            else saveRingPos (towerNr ++ [width]) (width - 1)

removeRingPos = undefined

                    

--removeRing height width = writeAt (1, (height-4)) (concat (take 3 (repeat ((concat (take (width `div` 2) (repeat "  "))) ++ "|" ++ (concat (take (width `div` 2) (repeat "  ")))))))

drawRings height width spacing = if (height == 0) then return ()
                                 else do drawRing height width spacing
                                         drawRings (height-1) width (spacing + 1)
                                           
drawRing height width spacing = writeAt (width, (height+1)) ((concat (take spacing (repeat " "))) ++ (concat (take height (repeat("# ")))))      
                  
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