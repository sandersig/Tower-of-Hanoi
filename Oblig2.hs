start moves height tower1 = do 
          c <- getLine
          let com = words c
          if(head com /= "q") then
              if(head com == "b") then do
                  clr
                  let moves = 0 
                  let height = read (head (tail com)) :: Int
                  if(height <= 1 || height > 12) then do
                      print "Unvalid nr of rings. The nr of rings needs to be between 2 and 12."
                      start 0 1 ringPos
                  else do
                    goto(0,0)
                    drawTowers (height+1) height
                    putStr "\ESC[0J"
                    drawRings height 0 1
                    goto(0, height+2)
                    start 1 height (saveRingPos ringPos 1 (height + 1) height)
              else do
                let f = read (head com) :: Int
                    t = read (last com) :: Int
                --doMove f t ringPos height
                goto(0, height+2) 
                putStrLn ("Number of moves: " ++ show moves)
                print ringPos
                start (moves + 1) height (saveRingPos ringPos 1 (height + 1) height)
          else return ()

-- dictionary med (tårnNr, posRing, breddePåRing) -> [(1, 5, 1), (1, 4, 2)]

saveRingPos ringPos towerNr height width = if (width == 0) then ringPos
                                           else saveRingPos (ringPos ++ [(towerNr, height, width)]) towerNr (height-1) (width-1)

--doMove f t ringPos height = saveRingPos ringPos t height                     

removeRing height width = writeAt (1, (height-4)) (concat (take 3 (repeat ((concat (take (width `div` 2) (repeat "  "))) ++ "|" ++ (concat (take (width `div` 2) (repeat "  ")))))))

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

--main :: IO ()
main = do putStrLn "Start a new game with: b <nbOfRings>, or quit with: q "
          start 0 1 []