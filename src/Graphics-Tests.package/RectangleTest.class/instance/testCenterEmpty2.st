tests
testCenterEmpty2

      | r1 c |
      r1 := 0@0 corner: 0@0.
		"does not create an empty rectangle"
      c := r1 center.
      self deny: (r1 containsPoint: c) description: 'An empty rectangle does not contain its center.'.
      self assert: (r1 topLeft dist: c) = (r1 bottomRight dist: c).
      self assert: (r1 bottomLeft dist: c) = (r1 topRight dist: c).
      self assert: (r1 topLeft dist: c) = (r1 bottomLeft dist: c).
      self assert: (r1 translateBy: -20@10) center = (c translateBy: -20@10) description: 'the center is translated with the rectangle'.