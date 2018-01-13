module BinSVG(
    Canvas,
    Coordinate,
    Color,
    Shape(Rectangle,Circle,Line,PolyLine),
    Screen(Screen),
    addShapeToScreen,
    writeScreenToSVG
)
where

-- A canvas is defined by a width and height
type Canvas = (Int,Int)

-- A coordinate is defined by a X  and Y value
type Coordinate = (Int,Int)

-- A color is defined by 3 values (RGB Format)
type Color = (Int,Int,Int)

-- The different possible shapes
data Shape = Rectangle Coordinate Int Int Color Int
            | Circle Coordinate Int Color Int
            | Line Coordinate Coordinate Color Int
            | PolyLine [Coordinate] Color Int
            | Polygon [Coordinate] Color Int
            deriving Show

-- Image is an array of Shape
data Screen = Screen Canvas [Shape] deriving Show

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

-- Useful only for polyline and polygons
coordArrayToString :: [Coordinate] -> String
coordArrayToString [] = ""
coordArrayToString ((hx,hy):t) = show hx ++ " " ++ show hy ++ " " ++ coordArrayToString t

shapeToString :: Shape -> String
shapeToString (Rectangle (x,y) width height color strokeW) = "<rect x='" ++ show x ++ "' y='" ++ show y ++ "' width='" ++ show width ++ "' height='" ++ show height ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='" ++ show strokeW ++ "'/>\n"
shapeToString (Circle (x,y) radius color strokeW) = "<circle cx='" ++ show x ++ "' cy='" ++ show y ++ "' r='" ++ show radius ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='" ++ show strokeW ++ "'/>\n"
shapeToString (Line (x1,y1) (x2,y2) color strokeW) = "<line x1='" ++ show x1 ++ "' x2='" ++ show x2 ++ "' y1='" ++ show y1 ++ "' y2='" ++ show y2 ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='" ++ show strokeW ++ "'/>\n"
shapeToString (PolyLine positions color strokeW) = "<polyline points='" ++ coordArrayToString positions ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='" ++ show strokeW ++ "'/>\n"
shapeToString (Polygon positions color strokeW) = "<polygon points='" ++ coordArrayToString positions ++ "' stroke='" ++ colorToString color ++ "' fill='transparent' stroke-width='" ++ show strokeW ++ "'/>\n"

shapeArrayToString :: [Shape] -> String
shapeArrayToString = foldr ((++) . shapeToString) ""

addShapeToScreen :: Shape -> Screen -> Screen
addShapeToScreen shape (Screen canvas shapes) = Screen canvas (shape : shapes)


-- Function to write an Image type data to a SVG file
writeScreenToSVG :: Screen -> String -> IO ()
writeScreenToSVG (Screen canvas shapes) path = writeFile path (headerSVG ++ canvasToString canvas ++ shapeArrayToString shapes ++ footerSVG)