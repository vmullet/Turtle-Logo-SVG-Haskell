module BinSVG(
    Canvas,
    Coordinate,
    Color,
    Shape(Rectangle,Circle,Line,PolyLine),
    Image(Image),
    addShapeToImage,
    writeImageToSVG
)
where

-- A canvas is defined by a width and height
type Canvas = (Int,Int)

-- A coordinate is defined by a X  and Y value
type Coordinate = (Int,Int)

-- A color is defined by 3 values (RGB Format)
type Color = (Int,Int,Int)

-- The different possible shapes
data Shape = Rectangle Coordinate Int Int Color 
            | Circle Coordinate Int Color
            | Line Coordinate Coordinate Color 
            | PolyLine [Coordinate] Color
            | Polygon [Coordinate] Color

-- Image is an array of Shape
data Image = Image Canvas [Shape]

-- Function for static parts of a SVG file
headerSVG :: String
headerSVG = "<?xml version='1.0' standalone='no'?>\n"

footerSVG :: String
footerSVG = "</svg>"

-- Function to convert the different abstract types to SVG String
canvasToString :: Canvas -> String
canvasToString (width,height) = "<svg width='" ++ show width ++ "' height='" ++ show height ++ "' version='1.1' xmlns='http://www.w3.org/2000/svg'>\n"

colorToString :: Color -> String
colorToString (red,green,blue) = "rgb(" ++ show red ++ ", " ++ show green ++ ", " ++ show blue ++ ")"

coordArrayToString :: [Coordinate] -> String
coordArrayToString [] = ""
coordArrayToString ((hx,hy):t) = show hx ++ " " ++ show hy ++ " " ++ coordArrayToString t

shapeToString :: Shape -> String
shapeToString (Rectangle (x,y) width height color) = "<rect x='" ++ show x ++ "' y='" ++ show y ++ "' width='" ++ show width ++ "' height='" ++ show height ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='5'/>\n"
shapeToString (Circle (x,y) radius color) = "<circle cx='" ++ show x ++ "' cy='" ++ show y ++ "' r='" ++ show radius ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='5'/>\n"
shapeToString (Line (x1,y1) (x2,y2) color) = "<line x1='" ++ show x1 ++ "' x2='" ++ show x2 ++ "' y1='" ++ show y1 ++ "' y2='" ++ show y2 ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='5'/>\n"
shapeToString (PolyLine positions color) = "<polyline points='" ++ coordArrayToString positions ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='5'/>\n"
shapeToString (Polygon positions color) = "<polygon points='" ++ coordArrayToString positions ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='5'/>\n"

shapeArrayToString :: [Shape] -> String
shapeArrayToString = foldr ((++) . shapeToString) ""

addShapeToImage :: Shape -> Image -> Image
addShapeToImage shape (Image canvas shapes) = Image canvas (shape : shapes)


-- Function to write an Image type data to a SVG file
writeImageToSVG :: Image -> String -> IO ()
writeImageToSVG (Image canvas shapes) path = writeFile path (headerSVG ++ canvasToString canvas ++ shapeArrayToString shapes ++ footerSVG)