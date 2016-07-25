tests
testRotateBy
	| rect |
	
	rect := Rectangle left:0  right:6  top:4  bottom:2. 

	self assert: (rect rotateBy:#left centerAt:0@0) left = 2.
	self assert: (rect rotateBy:#left centerAt:0@0) right = 4.
	self assert: (rect rotateBy:#left centerAt:0@0) top = -6.
	self assert: (rect rotateBy:#left centerAt:0@0) bottom = 0.
	
	self assert: (rect rotateBy:#right centerAt:0@0) left = -4.
	self assert: (rect rotateBy:#right centerAt:0@0) right = -2.
	self assert: (rect rotateBy:#right centerAt:0@0) top = 0.
	self assert: (rect rotateBy:#right centerAt:0@0) bottom = 6.
	
	self assert: (rect rotateBy:#pi centerAt:0@0) left = -6.
	self assert: (rect rotateBy:#pi centerAt:0@0) right = 0.
	self assert: (rect rotateBy:#pi centerAt:0@0) top = -4.
	self assert: (rect rotateBy:#pi centerAt:0@0) bottom = -2.