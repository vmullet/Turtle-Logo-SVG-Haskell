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

-- Function to draw a square
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
                        RP False,
                        MB halfLength,
                        RP True
                      ]
                    ]
                    where halfLength = Function "halfReduce" (Var "wingLength")

fractaleKoch :: Expr -> Order
fractaleKoch count = IF (count :==: Val 0) ([MF (Var "fractLength")],ifnot)
                     where  recur = fractaleKoch (count :-: Val 1)
                            ifnot = [
                                      recur,
                                      TL (Val 60),
                                      recur,
                                      TR (Val 120),
                                      recur,
                                      TL (Val 60),
                                      recur
                                    ]

drawSnowFlake :: World
drawSnowFlake = execProg  [
                            Build (1200,1200),
                            Declare
                            [
                              "n" := Val 4,
                              "baseLength" := Val 600,
                              -- For calculation
                              "3powerN" :-> (\x -> 3^x),
                              "fractLength" := ((Var "baseLength") :/: (Function "3powerN" (Var "n")))
                            ],
                            Repeat (Val 2)
                            [
                              fractale,
                              resetCount,
                              TR (Val 120)
                            ],
                            fractale,
                            resetCount
                          ]
                          where fractale = fractaleKoch (Var "n")
                                resetCount = Declare ["n" := Val 4]

-- Function to draw a spiral
drawSpiral :: World
drawSpiral = execProg [
                        Build (1200,1200),
                        Declare
                        [
                          "length" := Val 500,
                          "depth" := Val 300, -- Depth of the spiral (number of nested squares)
                          "reduceSquare" :-> (\x -> x - x `div` 20) -- Square length reduced of 5% every nested dquare
                        ],
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


main = do
    writeWorldToSVG drawSquare "C:\\Etc\\square.svg"
    writeWorldToSVG drawRegularPolygon "C:\\Etc\\polygReg.svg"
    writeWorldToSVG drawMill "C:\\Etc\\mill.svg"
    writeWorldToSVG drawSpiral "C:\\Etc\\mill.svg"
    writeWorldToSVG drawSnowFlake "C:\\Etc\\snowFlake.svg"