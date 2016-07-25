tests
testCenter 
	| r1 c |
	r1 := 0@0 extent: 10@20.
	c := r1 center.
	self assert: (r1 containsPoint: c) description: 'the center is inside the rectangle'.
	self assert: (r1 topLeft dist: c) = (r1 bottomRight dist: c).
	self assert: (r1 bottomLeft dist: c) = (r1 topRight dist: c).
	self assert: (r1 topLeft dist: c) = (r1 bottomLeft dist: c).
	self assert: (r1 translateBy: -20@10) center = (c translateBy: -20@10) description: 'the center is translated with the rectangle'