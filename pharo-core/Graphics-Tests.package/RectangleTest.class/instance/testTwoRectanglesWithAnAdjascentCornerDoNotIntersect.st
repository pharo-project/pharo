tests
testTwoRectanglesWithAnAdjascentCornerDoNotIntersect
  
   | rect1 rect2 |
   rect1 := 0@0 corner: 40@40.
   rect2 := 40@40 corner: 50@50.
   self deny: (rect1 intersects: rect2);
        deny: (rect2 intersects: rect1).