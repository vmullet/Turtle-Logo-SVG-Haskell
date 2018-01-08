module BinTurtle(
    Canvas,
    Cap,
    Var,
    Order(TL,TR,MF,MB,RP,Clear,Repeat,Declare,Build,IF),
    World(World),
    Expr(Var,Val,Function,(:+:),(:-:),(:*:),(:/:),(:==:),Neg),
    Stmt((:=),(:->)),
    execProg,
    writeWorldToSVG
)
where

import BinSVG
import Data.Maybe

-- The cap of the turtle in degree
type Cap = Int

-- Types related to variables / functions system
type Var = String -- A variable representation
type Val = Int -- The variable value representation
type Function = (Val -> Val)
type Memory = [(Var,Val)] -- Mapping between Variable and their values
type Engine = [(Var,Function)] -- Mapping between Functions and their values
type Storage = (Memory,Engine) -- Representation of the storage system of the World

-- An order given to the turtle
data Order = TL Expr  -- Turn Left angle
            | TR Expr -- Turn Right angle
            | MF Expr -- Move Forward pixels
            | MB Expr -- Move Backward pixels
            | RP Bool-- Rise Pen True/False
            | Clear -- Clear screen and center turtle
            | Repeat Expr [Order] -- Repeat a series of simple order (see above)
            | Declare [Stmt]
            | Build Canvas
            | IF Expr ([Order],[Order])

-- The turtle abstract data type
data Turtle = Turtle Coordinate Cap Bool

-- The world abstract data type
data World = World Turtle Image Storage

----- IMPLEMENTATION OF THE VARIABLE SYSTEM -----

data Expr = Var Var
          | Val Val
          | Function Var Expr -- The expression to execute a function (Var is the name of the function)
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :==: Expr
          | Neg Expr

instance Eq Expr where
    (Var a) == (Var b) = (a == b)
    (Val a) == (Val b) = (a == b)
    (Val a) == (Var b) = error "No compatible Expression"

-- Possible instructions
data Stmt = Var := Expr -- Declare and store variables in the memory
          | Var :-> Function -- Declare and store functions in the engine

-- Function to retrieve a function by the variable name in the engine
getFunction :: Var -> Engine -> Function
getFunction var engine = fromMaybe (error ("Variable not affected to a function ")) (lookup var engine)

boolToInt :: Bool -> Int
boolToInt b = if (b) then 1 else 0

-- Function to evaluate an expression
eval :: Expr -> Storage -> Val
eval (Val v) _ = v
eval (Var x) (memory,_) = fromMaybe (error ("Variable not affected to a value ")) (lookup x memory)
eval (Function name param) (m,e) = (getFunction name e) (eval param (m,e))
eval (e1 :+: e2) store = eval e1 store + eval e2 store
eval (e1 :-: e2) store = eval e1 store - eval e2 store
eval (e1 :*: e2) store = eval e1 store * eval e2 store
eval (e1 :/: e2) store = eval e1 store `div` eval e2 store
eval (Neg e1) store = -(eval e1 store)
eval (e1 :==: e2) store = boolToInt ((eval e1 store) == (eval e2 store))

-- Function to execute a single statement (The storage is updated)
execStmt :: Stmt -> Storage -> Storage
execStmt (x := expr) (m,e) = if (existMemVar x m) then (updateMemVar x (eval expr (m,e)) m, e) else ((x,eval expr (m,e)) : m, e) -- The variable is added or updated
execStmt (x :-> funct) (m,e) = (m,(x,funct) : e)  -- The engine is updated


-- Function to execute an array of statements
execStmts :: [Stmt] -> Storage -> Storage
execStmts t store = foldl (flip execStmt) store t
-------------------------------------------------

existMemVar :: Var -> Memory -> Bool
existMemVar var [] = False
existMemVar var ((name,_):t)  | (var == name) = True
                              | otherwise = existMemVar var t

updateMemVar :: Var -> Val -> Memory -> Memory
updateMemVar var newVal [] = []
updateMemVar var newVal ((name,val):t) = if (var == name) then (name,newVal):t else (name,val):updateMemVar var newVal t

-- Function to convert degrees to radians
toRadian :: Int -> Float
toRadian degree = fromIntegral degree * pi / 180.0

-- Function to create the world and set the turtle in the middle (used for Build and Clear Orders)
buildWorld :: Canvas -> World
buildWorld (width,height) = World(Turtle(quot width 2,quot width 2) 0 True) (Image (width,height) []) ([],[])

-- Function to create an emptyWorld (base parameter for the execOrders function)
emptyWorld :: World
emptyWorld = World(Turtle(0,0) 0 False) (Image (0,0) []) ([],[])


-- Function to execute an order by the turtle in the world
execOrder :: Order -> World -> World
execOrder (Build canvas) _ = buildWorld canvas -- Order to build the world
execOrder (TL angle) (World (Turtle (tx,ty) cap pen) image store) = World (Turtle (tx,ty) ((cap - eval angle store) `mod` 360) pen) image store
execOrder (TR angle) world = execOrder (TL (Neg angle)) world -- Turn Right = Opposite of Turn Left
execOrder (MF pixels) (World (Turtle (tx,ty) cap pen) image store) | pen = World (Turtle (endX,endY) cap pen) (addShapeToImage (Line (tx,ty) (endX,endY) (0,0,0)) image) store
                                                                   | otherwise = World (Turtle (endX,endY) cap pen) image store
                                                                     where endX = tx + round (cos (toRadian cap) * fromIntegral (eval pixels store))
                                                                           endY = ty + round (sin (toRadian cap) * fromIntegral (eval pixels store))

execOrder (MB pixels) world = execOrder (MF (Neg pixels)) world -- Move Backward = Opposite of Move Forward
execOrder (RP statePen) (World (Turtle (tx,ty) cap _) image store) = World (Turtle (tx,ty) cap statePen) image store
execOrder Clear (World _ (Image canvas _) _) = buildWorld canvas -- Reset screen and turtle

-- Add the possibility to repeat a list of orders
execOrder (Repeat count orders) (World turtle image storage) | valCount > 0 = execOrder (Repeat (Val (valCount-1)) orders) (execOrders world orders)
                                                             | otherwise = world
                                                             where world = World turtle image storage
                                                                   valCount = eval count storage 

-- Add the possibility to declare functions and variables
execOrder (Declare stmts) (World turtle image storage) = World turtle image (execStmts stmts storage)
execOrder (IF expr (ordersT,ordersF)) (World turtle image storage) = if (eval expr storage) == 1 then execOrders (World turtle image storage) ordersT else execOrders (World turtle image storage) ordersF


-- Function to execute a list of orders by the turtle in the world
execOrders :: World -> [Order] -> World
execOrders = foldl (flip execOrder)

-- The main function to execute a list of orders 
-- N.B : The baseWorld is an emptyWorld as everything will be initialized by a "Build" order)
execProg :: [Order] -> World
execProg [] = emptyWorld
execProg orders = execOrders emptyWorld orders

-- Function to export the world into a SVG file
writeWorldToSVG :: World -> String -> IO ()
writeWorldToSVG (World _ image _) = writeImageToSVG image -- Check binSvg for the writeImageToSVG function



