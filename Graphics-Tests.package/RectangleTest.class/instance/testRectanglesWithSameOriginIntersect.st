tests
testRectanglesWithSameOriginIntersect

    | rect1 rect2 |
   rect1 := 10@10 corner: 20@30.
   rect2 := rect1 corner extent: 20@40.
   self deny: (rect1 intersects: rect2).