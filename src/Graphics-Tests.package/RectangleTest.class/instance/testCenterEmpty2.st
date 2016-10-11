tests
testCenterEmpty2

      | r1 c |
      r1 := 0@0 corner: 0@0.
		"does not create an empty rectangle"
      c := r1 center.
      self deny: (r1 containsPoint: c) description: 'An empty rectangle does not contain its center.'.
      self assert: (r1 topLeft distanceTo: c) = (r1 bottomRight distanceTo: c).
      self assert: (r1 bottomLeft distanceTo: c) = (r1 topRight distanceTo: c).
      self assert: (r1 topLeft distanceTo: c) = (r1 bottomLeft distanceTo: c).
      self assert: (r1 translateBy: -20@10) center = (c translateBy: -20@10) description: 'the center is translated with the rectangle'.