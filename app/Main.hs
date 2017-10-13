module Main where

import           Graphics.Implicit

letter height stroke 'J' = union [translate (or, or) semi, vertBar, horzBar]
  where semi = difference [ring ir or, rectR 0 (-or, 0) (or, or)]
        width = height * 0.6
        innerDiameter = width - stroke*2
        outterDiameter = width
        (ir, or) = (innerDiameter/2, outterDiameter/2)
        horzBar = rectR 0 (stroke, height-stroke*0.75) (width, height)
        vertBar = rectR 0 (width - stroke, or-1) (width, height)
letter height stroke 'E' = union [top, middle, bottom, vert]
  where top = rectR 0 (0, height-stroke) (width, height)
        middle = rectR 0 (0, height-stroke*2.75) (width-stroke*0.25, height-stroke*1.75)
        bottom = rectR 0 (0, 0) (width, stroke)
        vert = rectR 0 (0, 0) (stroke, height)
        width = height * 0.6
letter height stroke 'M' = union [left, middleV, right]
  where left = rectR 0 (0, 0) (stroke, height)
        right = rectR 0 (width-stroke, 0) (width, height)
        middleV = polygonR 0 [(0, height), (stroke, height), (width/2, (height+stroke)/2), (width-stroke, height), (width, height), (width/2, (height-stroke)/2)]
        width = height * 0.6

string height stroke letterSpacing str = union $ zipWith (\i x -> translate (i * (height*0.6 + letterSpacing), 0) $ letter height stroke x) [0..] str

stringWidth :: Double -> Double -> String -> Double
stringWidth height letterSpacing str = height * 0.6 * len + letterSpacing * (len - 1)
  where len = fromIntegral $ length str


ring ir or = difference [circle or, circle ir]



stringPlaque str = union [ rect3R 0 (0,0,0) (stringWidth height spacing str + padding * 2, height + padding * 2, depth)
                         , translate (padding, padding, 0) $ extrudeR 0 (string 40 8 5 "JEM") (depth * 2)
                         ]
  where height = 40
        stroke = 8
        spacing = 5
        padding = 10
        depth = 5


main = writeSTL 2 "test.stl" (stringPlaque "JEM")
