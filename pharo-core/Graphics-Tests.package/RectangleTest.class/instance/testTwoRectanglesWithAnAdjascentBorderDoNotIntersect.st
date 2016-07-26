tests
testTwoRectanglesWithAnAdjascentBorderDoNotIntersect
  
   | rect1 rect2 |
   rect1 := 0@0 corner: 40@40.
   rect2 := 40@0 extent:40@40.
   self deny: (rect1 intersects: rect2);
        deny: (rect2 intersects: rect1).