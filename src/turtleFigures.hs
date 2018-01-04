import BinTurtle

-- Function to draw a square
-- Canvas : The world resolution
-- Int : The LengthSide of the square
drawSquare :: Canvas -> Int -> World
drawSquare canvas lengthSide = execOrders (buildWorld canvas) [Repeat 4 [MF lengthSide,TL 90]]

-- Function to draw any regular polygon whatever the number of sides
-- Canvas : The world resolution
-- Int : The number of side of this polygon
-- Int : The length of each side
drawRegularPolygon :: Canvas -> Int -> Int -> World
drawRegularPolygon canvas nbSide lengthSide = execOrders (buildWorld canvas) [Repeat nbSide [MF lengthSide,TR (round(360.0 / fromIntegral nbSide))]]

-- Function to draw a mill
-- Canvas : The world resolution
-- Int : The length of each wings
-- #Note : I suppose that the square part start from the middle of the wing
drawMill :: Canvas -> Int -> World
drawMill canvas wingLength = execOrders (buildWorld canvas) [TL 45,Repeat 4 [TR 90,MF wingLength,Repeat 3 [TR 90,MF halfLength],TR 90,RP False,MB halfLength,RP True]]
                             where halfLength = quot wingLength 2


-- Function to generate the fractale of Knock (1 fragment of the snowflake)
-- Int : Recursive counter
-- Int : Total Length of the fractale (at n = 0)
fractaleKnock :: Int -> Int -> [Order]
fractaleKnock n fractLength | n == 0 = [MF fractLength]
                           | otherwise = recur ++ [TL 60] ++ recur ++ [TR 120] ++ recur ++ [TL 60] ++ recur
                           where recur = fractaleKnock (n-1) (round(fromIntegral fractLength / 3.0))

-- Function to draw the snowFlake by using the fractale function
-- Int : Recursive counter
-- Int : Total Length of the fractale (at n = 0)
drawSnowFlake :: Canvas -> Int -> Int -> World
drawSnowFlake canvas n fractLength = execOrders (buildWorld canvas) ([Repeat 2 (fractale ++ [TR 120])] ++ fractale)
                                     where fractale = fractaleKoch n fractLength


main = do
    let square = drawSquare (500,500) 150
    let approxCirc = drawRegularPolygon (1300,1300) 24 75
    let mill = drawMill (800,800) 300
    let snowFlake = drawSnowFlake (1200,1200) 4 600
    writeWorldToSVG square "square.svg"
    writeWorldToSVG approxCirc "polygReg.svg"
    writeWorldToSVG mill "mill.svg"
    writeWorldToSVG snowFlake "snowFlake.svg"