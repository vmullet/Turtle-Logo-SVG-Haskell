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

main = do
    --let snowFlake = drawSnowFlake (1200,1200) 4 600
    writeWorldToSVG drawSquare "C:\\Etc\\square.svg"
    writeWorldToSVG drawRegularPolygon "C:\\Etc\\polygReg.svg"
    writeWorldToSVG drawMill "C:\\Etc\\mill.svg"
    --writeWorldToSVG snowFlake "C:\\Etc\\snowFlake.svg"