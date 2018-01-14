import BinSVG
import System.Random

-- Function to generate a random Color
getRandomColor :: IO (Color)
getRandomColor = do
    red <- randomRIO(0,255)
    green <- randomRIO(0,255)
    blue <- randomRIO(0,255)
    return (red,green,blue)

-- Function to generate a random width stroke
getRandomStroke :: IO (Int)
getRandomStroke = do
    stroke <- randomRIO(1,10)
    return stroke

-- Function to generate a Random Square
getRandomSquare :: Canvas -> IO (Shape)
getRandomSquare (width,_) = do
    sqPos <- randomRIO(0,width) -- X and Y of the randomSquare
    sqW <- randomRIO(1,width - sqPos) -- Width of the randomSquare
    randomColor <- getRandomColor
    randomStroke <- getRandomStroke
    return (Rectangle (sqPos,sqPos) sqW sqW randomColor randomStroke)


-- Function to generate X random squares on a given canvas
-- Screen : The screen where the squares will be drawn (will be filled recursively)
-- Int : The number of squares to generate
-- String : The path of the SVG file to generate
drawRandomSquares :: Screen -> Int -> String -> IO ()
drawRandomSquares (Screen canvas shapes) quantity path  | quantity < 0 = error "The number of square to generate must be positive"
                                                        | quantity > 0 = do
                                                            randomSquare <- getRandomSquare canvas
                                                            drawRandomSquares (Screen canvas (randomSquare:shapes)) (quantity - 1) path
                                                        | otherwise = writeScreenToSVG (Screen canvas shapes) path


main = do
  drawRandomSquares (Screen (1000,1000) []) 10 "..\\svg\\randomSquares.svg"
