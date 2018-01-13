# Turtle Logo Haskell-SVG

This project consists in creating an abstract language to reproduce the **Turtle Logo** in **Haskell** by using the **SVG** format as a graphical interface.
With the features of functional programming, it will be possible to draw complex figures easily such as **fractales (Koch Snowflake,etc...)** by giving orders to the turtle.
This abstract language will be called : **The Turtle Language**


## Project Structure

This project contains 2 folders : **"src"** and **"svg"**

 The folder **src** contains the source code :
 
1. **binSVG.hs**

This file is a module containing all functions / abstract data types related to the SVG export

2. **binTurtle.hs** 

This file is another module containing all functions related to the turtle (orders...) and the definition of the abstract language (variables,functions,expressions)

3. **randomSquares.hs**

This file is just an example about how to draw random squares on a SVG file by using the module *binSVG*

4. **turtleFigures.hs**

This file contains examples of figures (more or less complex) which can be drawn with the abstract language created.

You can find the result of these functions in the ***svg*** folder.
