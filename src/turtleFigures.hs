import BinTurtle

-- Function to draw a square
drawSquare :: World
drawSquare = execProg [
                        Build (500,500),
                        Declare
                        [
                          "length" := Val 150
                        ],
                        Repeat (Val 4)
                        [
                          MF (Var "length"),
                          TL (Val 90)
                        ]
                      ]

-- Function to draw a square (Example with a dynamic Repeat order)
drawSquare2 :: World
drawSquare2 = execProg [
                        Build (500,500),
                        Declare
                        [
                          "length" := Val 150,
                          "count" := Val 5
                        ],
                        Repeat (Var "count" :-: Val 1)
                        [
                          MF (Var "length"),
                          TL (Val 90)
                        ]
                      ]

-- Function to draw any regular polygon whatever the number of sides
drawRegularPolygon :: World
drawRegularPolygon = execProg [
                                Build (1300,1300),
                                Declare
                                [
                                  "lengthSide" := Val 75,
                                  "nbSide" := Val 24,
                                  "getAngle" :-> (\x -> 360 `div` x)
                                ],
                                Repeat (Var "nbSide")
                                [
                                  MF (Var "lengthSide"),
                                  TL (Function "getAngle" (Var "nbSide"))
                                ]
                              ]

-- Function to draw a mill
drawMill :: World
drawMill = execProg [
                      Build (800,800),
                      Declare
                      [
                        "wingLength" := Val 300,
                        "halfReduce" :-> (\x -> x `div` 2)
                      ],
                      TL (Val 45),
                      Repeat (Val 4)
                      [
                        TR (Val 90),
                        MF (Var "wingLength"),
                        Repeat (Val 3)
                        [
                          TR (Val 90),
                          MF halfLength
                        ],
                        TR (Val 90),
                        LP False,
                        MB halfLength,
                        LP True
                      ]
                    ]
                    where halfLength = Function "halfReduce" (Var "wingLength")

----------------------------- The Koch SnowFlake and some variants ------------------------------------
fractaleKoch :: Expr -> Order
fractaleKoch count = IF (count :==: Val 0) ([MF (Var "fractLength")],ifnot)
                     where  recur = fractaleKoch (count :-: Val 1) -- Decrease currentN
                            ifnot = [
                                      recur,
                                      TL (Val 60),
                                      recur,
                                      TR (Val 120),
                                      recur,
                                      TL (Val 60),
                                      recur
                                    ]

quadraKoch :: Expr -> Order
quadraKoch count = IF (count :==: Val 0) ([MF (Var "fractLength")],ifnot)
                     where  recur = quadraKoch (count :-: Val 1) -- Decrease currentN
                            ifnot = [
                                      recur,
                                      TL (Val 90),
                                      recur,
                                      TR (Val 90),
                                      recur,
                                      TR (Val 90),
                                      recur,
                                      TL (Val 90),
                                      recur
                                    ]

pointedKoch :: Expr -> Order
pointedKoch count = IF (count :==: Val 0) ([MF (Var "fractLength")],ifnot)
                     where  recur = fractaleKoch (count :-: Val 1) -- Decrease currentN
                            ifnot = [
                                      recur,
                                      TL (Val 90),
                                      recur,
                                      TR (Val 180),
                                      recur,
                                      TL (Val 90),
                                      recur
                                    ]

invertedKoch :: Expr -> Order
invertedKoch count = IF (count :==: Val 0) ([MF (Var "fractLength")],ifnot)
                     where  recur = fractaleKoch (count :-: Val 1) -- 
                            ifnot = [
                                      recur,
                                      TL (Val 120),
                                      recur,
                                      TL (Val 120),
                                      recur,
                                      TL (Val 120),
                                      recur
                                    ]

-- Function to draw the snowflake based on the fractaleKoch function above
drawSnowFlake :: World
drawSnowFlake = execProg  [
                            Build (1200,1200),
                            Declare
                            [
                              "constN" := Val 4, -- The n generation desired
                              "baseLength" := Val 600, -- The total fractale length
                              -- For calculation, mustn't be edited
                              "3powerN" :-> (\x -> 3^x),
                              "currentN" := Var "constN", -- The current value of n (initialized to constN value)
                              "fractLength" := (Var "baseLength" :/: (Function "3powerN" (Var "constN"))) -- The length of every fractale fragment
                            ],
                            Ink (150,200,255),
                            Repeat (Val 3)
                            [
                              fractaleKoch (Var "currentN"),
                              TR (Val 120),
                              Declare ["currentN" := Var "constN"] -- Reset the current n value
                            ]
                          ]

drawQuadraSnowFlake :: World
drawQuadraSnowFlake = execProg  [
                            Build (1500,1500),
                            Declare
                            [
                              "constN" := Val 4, -- The n generation desired
                              "baseLength" := Val 400, -- The total fractale length
                              -- For calculation, shouldn't be edited
                              "3powerN" :-> (\x -> 3^x),
                              "currentN" := Var "constN", -- The current value of n (initialized to constN value)
                              "fractLength" := (Var "baseLength" :/: (Function "3powerN" (Var "constN"))) -- The length of every fractale fragment
                            ],
                            Ink (150,200,255),
                            Repeat (Val 4)
                            [
                              quadraKoch (Var "currentN"),
                              TR (Val 90),
                              Declare ["currentN" := Var "constN"] -- Reset the current n value
                            ]
                          ]

drawPointedSnowFlake :: World
drawPointedSnowFlake = execProg  [
                                    Build (1500,1500),
                                    Declare
                                    [
                                      "constN" := Val 4, -- The n generation desired
                                      "baseLength" := Val 700, -- The total fractale length
                                      -- For calculation, mustn't be edited
                                      "3powerN" :-> (\x -> 3^x),
                                      "currentN" := Var "constN", -- The current value of n (initialized to constN value)
                                      "fractLength" := (Var "baseLength" :/: (Function "3powerN" (Var "constN"))) -- The length of every fractale fragment
                                    ],
                                    Ink (150,200,255),
                                    Repeat (Val 4)
                                    [
                                      pointedKoch (Var "currentN"),
                                      TR (Val 120),
                                      Declare ["currentN" := Var "constN"] -- Reset the current n value
                                    ]
                                  ]

drawInvertedSnowFlake :: World
drawInvertedSnowFlake = execProg  [
                                    Build (1200,1200),
                                    Declare
                                    [
                                      "constN" := Val 4, -- The n generation desired
                                      "baseLength" := Val 1600, -- The total fractale length
                                      -- For calculation, mustn't be edited
                                      "3powerN" :-> (\x -> 3^x),
                                      "currentN" := Var "constN", -- The current value of n (initialized to constN value)
                                      "fractLength" := (Var "baseLength" :/: (Function "3powerN" (Var "constN"))) -- The length of every fractale fragment
                                    ],
                                    Ink (150,200,255),
                                    Repeat (Val 4)
                                    [
                                      invertedKoch (Var "currentN"),
                                      TL (Val 120),
                                      Declare ["currentN" := Var "constN"] -- Reset the current n value
                                    ]
                                  ]

---------------------------------------------------------------------------------------------

-- Functions to draw spirals (Quadratic or Circular)
drawQuadSpiral :: World
drawQuadSpiral = execProg [
                        Build (1200,1200),
                        Declare
                        [
                          "length" := Val 500,
                          "depth" := Val 100, -- Depth of the spiral (number of nested squares)
                          "reduceSquare" :-> (\x -> x - x `div` 20) -- Square length reduced of 5% every nested dquare
                        ],
                        Ink (80,0,80),
                        Stroke 4,
                        TL (Val 90),
                        Repeat (Var "depth")
                        [
                          Repeat (Val 2)
                          [
                            MF (Var "length"),
                            TR (Val 90)
                          ],
                          Declare
                          [
                            "length" := Function "reduceSquare" (Var "length") -- Update length variable
                          ]
                        ]
                      ]

drawCircSpiral :: World
drawCircSpiral = execProg [
                            Build (1200,1200),
                            Declare
                            [
                              "space" := Val 1,
                              "increase" := Val 1,
                              "depth" := Val 200
                            ],
                            Ink (255,100,100),
                            Stroke 20,
                            Repeat (Var "depth")
                            [
                              TL (Val 30),
                              MF (Var "space"),
                              Declare
                              [
                                "space" := ((Var "space") :+: Var "increase")
                              ]
                            ]
                          ]

-------------------- FUNCTIONS to draw the triangle of Sierpinski --------------------------------------------

sierpinski :: Expr -> Expr -> Order
sierpinski n long = IF (n :==: Val 0) (iftrue,ifnot)
                    where iftrue = [Repeat (Val 3) [MF long,TL (Val 120)]]
                          ifnot = [
                                    sierpinski (n :-: Val 1) (long :/: Val 2),
                                    MF (long :/: Val 2),
                                    sierpinski (n :-: Val 1) (long :/: Val 2),
                                    MB (long :/: Val 2),
                                    TL (Val 60),
                                    MF (long :/: Val 2),
                                    TR (Val 60),
                                    sierpinski (n :-: Val 1) (long :/: Val 2),
                                    TL (Val 60),
                                    MB (long :/: Val 2),
                                    TR (Val 60)
                                  ]

drawSierpinski :: World
drawSierpinski = execProg [
                            Build (1200,1200),
                            Declare
                            [
                              "n" := Val 4,
                              "length" := Val 600
                            ],
                            Ink (100,0,255),
                            Stroke 10,
                            sierpinski (Var "n") (Var "length")
                          ]

-------------------------------------------------------------------------------------------------------

main = do
    writeWorldToSVG drawSquare "..\\svg\\square.svg"
    writeWorldToSVG drawRegularPolygon "..\\svg\\polygReg.svg"
    writeWorldToSVG drawMill "..\\svg\\mill.svg"
    writeWorldToSVG drawQuadSpiral "..\\svg\\quadSpiral.svg"
    writeWorldToSVG drawCircSpiral "..\\svg\\circSpiral.svg"
    writeWorldToSVG drawSnowFlake "..\\svg\\snowFlake.svg"
    writeWorldToSVG drawQuadraSnowFlake "..\\svg\\quadraSnowFlake.svg"
    writeWorldToSVG drawPointedSnowFlake "..\\svg\\pointedSnowFlake.svg"
    writeWorldToSVG drawInvertedSnowFlake "..\\svg\\invertedSnowFlake.svg"
    writeWorldToSVG drawSierpinski "..\\svg\\triangleSierpinski.svg"