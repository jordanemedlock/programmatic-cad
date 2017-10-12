module Main where

import Graphics.Implicit

letter height 'J' = semi
  where semi = union [ring ir or, rectR 0 (-or, -or) (or, -or)]
        width = height
        strokeWidth = width / 10
        innerDiameter = width/2 - strokeWidth
        outterDiameter = width/2 + strokeWidth
        (ir, or) = (innerDiameter/2, outterDiameter/2)


ring ir or = difference [circle or, circle ir]

plate w h d = rect3R 0 (0,0,0) (w, h, d)

out = union [letter 40 'J']

main = writeSVG 2 "test.svg" out
