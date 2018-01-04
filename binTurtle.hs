module BinTurtle(
    Canvas,
    Cap,
    Order(TL,TR,MF,MB,RP,Clear,Repeat),
    Turtle(Turtle),
    World(World),
    buildWorld,
    execOrders,
    writeWorldToSVG
)
where

import BinSVG

-- The cap of the turtle in degree
type Cap = Int

-- An order given to the turtle
data Order = TL Int  -- Turn Left angle
            | TR Int -- Turn Right angle
            | MF Int -- Move Forward pixels
            | MB Int -- Move Backward pixels
            | RP Bool-- Rise Pen True/False
            | Clear -- Clear screen and center turtle
            | Repeat Int [Order] -- Repeat a series of simple order (see above)

-- The turtle abstract data type
data Turtle = Turtle Coordinate Cap Bool

-- The world abstract data type
data World = World Turtle Image

-- Function to convert degrees to radians
toRadian :: Int -> Float
toRadian degree = fromIntegral degree * pi / 180.0

-- Function to create the world and set the turtle in the middle
buildWorld :: Canvas -> World
buildWorld (width,height) = World(Turtle(quot width 2,quot width 2) 0 True) (Image (width,height) [])


-- Function to execute an order by the turtle in the world
execOrder :: Order -> World -> World
execOrder (TL angle) (World (Turtle (tx,ty) cap pen) image) = World (Turtle (tx,ty) ((cap-angle) `mod` 360) pen) image
execOrder (TR angle) (World (Turtle (tx,ty) cap pen) image) = World (Turtle (tx,ty) ((cap+angle) `mod` 360) pen) image
execOrder (MF pixels) (World (Turtle (tx,ty) cap pen) image) | pen = World (Turtle (endX,endY) cap pen) (addShapeToImage (Line (tx,ty) (endX,endY) (0,0,0)) image)
                                                             | otherwise = World (Turtle (endX,endY) cap pen) image
                                                              where endX = tx + round (cos (toRadian cap) * fromIntegral pixels)
                                                                    endY = ty + round (sin (toRadian cap) * fromIntegral pixels)
execOrder (MB pixels) (World (Turtle (tx,ty) cap pen) image) | pen = World (Turtle (endX,endY) cap pen) (addShapeToImage (Line (tx,ty) (endX,endY) (0,0,0)) image)
                                                             | otherwise = World (Turtle (endX,endY) cap pen) image
                                                              where endX = tx + round (cos (toRadian cap) * fromIntegral (-pixels))
                                                                    endY = ty + round (sin (toRadian cap) * fromIntegral (-pixels))
execOrder (RP statePen) (World (Turtle (tx,ty) cap _) image) = World (Turtle (tx,ty) cap statePen) image
execOrder Clear (World _ (Image canvas _)) = buildWorld canvas -- Reset screen and turtle

-- Add the possibility to repeat a list of orders
execOrder (Repeat count orders) world | count > 0 = execOrder (Repeat (count-1) orders) (execOrders world orders)
                                      | otherwise = world 

-- Function to execute a list of orders by the turtle in the world
execOrders :: World -> [Order] -> World
execOrders = foldl (flip execOrder)

-- Function to export the world into a SVG file
writeWorldToSVG :: World -> String -> IO ()
writeWorldToSVG (World _ image) = writeImageToSVG image



