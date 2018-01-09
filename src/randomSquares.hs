import BinSVG
import System.Random

-- Function to generate a random Color
getRandomColor :: IO (Color)
getRandomColor = do
    red <- randomRIO(0,255)
    green <- randomRIO(0,255)
    blue <- randomRIO(0,255)
    return (red,green,blue)

-- Function to generate a Random Square
getRandomSquare :: Canvas -> IO (Shape)
getRandomSquare (width,_) = do
    sqPos <- randomRIO(0,width) -- X and Y of the randomSquare
    sqW <- randomRIO(1,width - sqPos) -- Width of the randomSquare
    randomColor <- getRandomColor
    return (Rectangle (sqPos,sqPos) sqW sqW randomColor)


-- Function to generate X random squares on a given canvas (recursive)
-- Canvas: The canvas where the squares are drawn
-- [Shape] : The array of shapes generated (empty when executed first time : [])
-- Int : The number of squares to generate
drawRandomSquares :: Canvas -> [Shape] -> Int -> IO ()
drawRandomSquares canvas shapes quantity = do
    if (quantity > 0) then do
      randomSquare <- getRandomSquare canvas
      drawRandomSquares canvas (randomSquare:shapes) (quantity - 1)
    else 
        writeImageToSVG (Image canvas shapes) "randomSquares.svg"


main = do
  drawRandomSquare (1000,1000) [] 10
