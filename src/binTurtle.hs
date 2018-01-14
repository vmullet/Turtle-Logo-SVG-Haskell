module BinTurtle(
    Canvas,
    Cap,
    Pen,
    Var,
    Order(TL,TR,MF,MB,LP,Ink,Stroke,Clear,Repeat,Declare,Build,IF),
    World(World),
    Expr(Var,Val,Function,(:+:),(:-:),(:*:),(:/:),(:==:),(:!=:),(:>:),(:<:),(:>=:),(:<=:),Neg),
    Stmt((:=),(:->)),
    execProg,
    writeWorldToSVG
)
where

import BinSVG
import Data.Maybe

-- The cap of the turtle in degree
type Cap = Int
type Pen = (Bool,Color,Int) -- The pen is defined by (Lowered or not, its color and its stroke width)

-- Types related to variables / functions system
type Var = String -- A variable representation
type Val = Int -- The variable value representation
type Function = (Val -> Val) -- A function representation
type Memory = [(Var,Val)] -- Mapping between Variable and their values
type Engine = [(Var,Function)] -- Mapping between Functions and their definitions
type Storage = (Memory,Engine) -- Representation of the storage system of the World

-- Orders which can be given to the turtle
data Order = TL Expr  -- Turn Left angle
            | TR Expr -- Turn Right angle
            | MF Expr -- Move Forward pixels
            | MB Expr -- Move Backward pixels
            | LP Bool-- Lower Pen : If True, leave a trace
            | Ink Color -- Change Pen Ink color
            | Stroke Int -- Change the stroke width of the pen
            | Clear -- Clear screen and center turtle
            | Repeat Expr [Order] -- Repeat a series of orders for a specific number of times under the form of an expression
            | Declare [Stmt] -- Declare variables,functions,update their values...
            | Build Canvas -- Build the screen and init turtle position
            | IF Expr ([Order],[Order]) -- IF Condition ([OrdersIfTrue],[OrdersIfFalse])
            deriving Show

-- The turtle abstract data type
data Turtle = Turtle Coordinate Cap Pen deriving Show

-- The world abstract data type
data World = World Turtle Screen Storage

----- IMPLEMENTATION OF THE VARIABLE SYSTEM -----

data Expr = Var Var
          | Val Val
          | Function Var Expr -- The expression to execute a function (Var is the name of the function and Expr the parameter)
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :==: Expr -- Equal
          | Expr :!=: Expr -- Not Equal
          | Expr :>: Expr
          | Expr :<: Expr
          | Expr :>=: Expr
          | Expr :<=: Expr
          | Neg Expr -- Negative (is like *(-1))
          deriving Show

-- Possible instructions / Statements
data Stmt = Var := Expr -- Declare and store variables in the memory
          | Var :-> Function -- Declare and store functions in the engine

-- Defined because you can't show a function definition, only a function result
instance Show Stmt where
  show (x :-> _) = x ++ " -> Function"
  show (x := expr) = show x ++ " := " ++ show expr

-- Function to convert bool into int (The language only supports Int)
boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

-- Function to retrieve a function by the variable name in the engine
getFunction :: Var -> Engine -> Function
getFunction var engine = fromMaybe (error "Variable not affected to a function ") (lookup var engine)

-- Function to evaluate an expression
eval :: Expr -> Storage -> Val
eval (Val v) _ = v
eval (Var x) (memory,_) = fromMaybe (error "Variable not affected to a value ") (lookup x memory)
eval (Function name param) (m,e) = getFunction name e (eval param (m, e)) -- Get function and apply it to parameter
eval (e1 :+: e2) store = eval e1 store + eval e2 store
eval (e1 :-: e2) store = eval e1 store - eval e2 store
eval (e1 :*: e2) store = eval e1 store * eval e2 store
eval (e1 :/: e2) store = eval e1 store `div` eval e2 store
eval (e1 :==: e2) store = boolToInt (eval e1 store == eval e2 store)
eval (e1 :!=: e2) store = boolToInt (eval e1 store /= eval e2 store)
eval (e1 :>: e2) store = boolToInt (eval e1 store > eval e2 store)
eval (e1 :<: e2) store = boolToInt (eval e1 store < eval e2 store)
eval (e1 :>=: e2) store = boolToInt (eval e1 store >= eval e2 store)
eval (e1 :<=: e2) store = boolToInt (eval e1 store <= eval e2 store)
eval (Neg e1) store = -(eval e1 store)


-- Function to execute a single statement (The storage is updated)
execStmt :: Stmt -> Storage -> Storage
execStmt (x := expr) (m,e) = if existMemVar x m then (updateMemVar x newValue m, e)
                                                else ((x, newValue) : m, e) -- The variable is added or updated
                             where newValue = eval expr (m, e)

execStmt (x :-> funct) (m,e) = if existEngFunc x e then (m, updateEngFunc x funct e)
                                                   else(m,(x,funct) : e)  -- The function is added or updated in the engine


-- Function to execute an array of statements
execStmts :: [Stmt] -> Storage -> Storage
execStmts t store = foldl (flip execStmt) store t
-------------------------------------------------

-- Function to check if the variable already exists in the memory
existMemVar :: Var -> Memory -> Bool
existMemVar _ [] = False
existMemVar var ((name,_):t)  | var == name = True
                              | otherwise = existMemVar var t


-- Function to update the variable value in the memory
updateMemVar :: Var -> Val -> Memory -> Memory
updateMemVar _ _ [] = []
updateMemVar var newVal ((name,val):t) = if var == name then (name, newVal) : t 
                                                        else (name, val) : updateMemVar var newVal t

-- Function to check if the function already exists in the engine
existEngFunc :: Var -> Engine -> Bool
existEngFunc _ [] = False
existEngFunc var ((name,_):t)  | var == name = True
                               | otherwise = existEngFunc var t

-- Function to update the function definition in the engine
updateEngFunc :: Var -> Function -> Engine -> Engine
updateEngFunc _ _ [] = []
updateEngFunc var newFunc ((name,func):t) = if var == name then (name, newFunc) : t 
                                                           else (name, func) : updateEngFunc var newFunc t

-- Function to convert degrees to radians
toRadian :: Int -> Float
toRadian degree = fromIntegral degree * pi / 180.0

-- Function to get the default pen configuration (lowered,color = black and stroke width = 5)
defaultPen :: Pen
defaultPen = (True,(0,0,0),5)

-- Function to create the world and set the turtle in the middle with the pen lowered (used for Build and Clear Orders)
buildWorld :: Canvas -> World
buildWorld (width,height) = World(Turtle(tPos,tPos) 0 defaultPen) (Screen (width,height) []) ([],[])
                            where tPos = width `div` 2

-- Function to create an emptyWorld (no canvas,no turtle in the middle...) (base parameter for the execOrders function)
emptyWorld :: World
emptyWorld = World(Turtle(0,0) 0 defaultPen) (Screen (0,0) []) ([],[])


-- Function to execute an order by the turtle in the world
execOrder :: Order -> World -> World
execOrder (Build canvas) _ = buildWorld canvas -- Order to build the world and init turtle position
execOrder (TL angle) (World (Turtle (tx,ty) cap pen) screen store) = World (Turtle (tx,ty) ((cap - eval angle store) `mod` 360) pen) screen store
execOrder (TR angle) world = execOrder (TL (Neg angle)) world -- Turn Right = Opposite of Turn Left
execOrder (MF pixels) (World (Turtle (tx,ty) cap (lowerPen,color,stroke)) screen store) | lowerPen = World (Turtle (endX,endY) cap pen) (addShapeToScreen (Line (tx,ty) (endX,endY) color stroke) screen) store
                                                                                        | otherwise = World (Turtle (endX,endY) cap pen) screen store
                                                                                        where endX = tx + round (cos (toRadian cap) * fromIntegral (eval pixels store))
                                                                                              endY = ty + round (sin (toRadian cap) * fromIntegral (eval pixels store))
                                                                                              pen = (lowerPen,color,stroke)

execOrder (MB pixels) world = execOrder (MF (Neg pixels)) world -- Move Backward = Opposite of Move Forward
execOrder (LP lowerPen) (World (Turtle (tx,ty) cap (_,penColor,stroke)) screen store) = World (Turtle (tx,ty) cap (lowerPen,penColor,stroke)) screen store
execOrder (Ink color) (World (Turtle (tx,ty) cap (lowerPen,_,stroke)) screen store) = World (Turtle (tx,ty) cap (lowerPen,color,stroke)) screen store
execOrder (Stroke stroke) (World (Turtle (tx,ty) cap (lowerPen,color,_)) screen store) = World (Turtle (tx,ty) cap (lowerPen,color,stroke)) screen store
execOrder Clear (World _ (Screen canvas _) _) = buildWorld canvas -- Reset screen and turtle

-- Add the possibility to repeat a list of orders
execOrder (Repeat count orders) (World turtle screen storage) | valCount < 0 = error "The repeat count must be positive"
                                                              | valCount > 0 = execOrder (Repeat (Val (valCount-1)) orders) (execOrders world orders)
                                                              | otherwise = world
                                                              where world = World turtle screen storage
                                                                    valCount = eval count storage 

-- Add the possibility to declare/update functions and variables
execOrder (Declare stmts) (World turtle screen storage) = World turtle screen (execStmts stmts storage)

-- Add the possibility to give IF condition
execOrder (IF expr (ordersT,ordersF)) (World turtle screen storage) = if eval expr storage == 1 then execOrders (World turtle screen storage) ordersT
                                                                                                else execOrders (World turtle screen storage) ordersF


-- Function to execute a list of orders by the turtle in the world
execOrders :: World -> [Order] -> World
execOrders = foldl (flip execOrder)

-- The main function to execute a list of orders 
-- The first order must be a "Build" order inside the array of Orders)
execProg :: [Order] -> World
execProg [] = emptyWorld
execProg orders = execOrders emptyWorld orders

-- Function to export the world into a SVG file
writeWorldToSVG :: World -> String -> IO ()
writeWorldToSVG (World _ screen _) = writeScreenToSVG screen -- Check binSvg for the writeScreenToSVG function



