tests
testInsettingSmallerThanRectangleExtentShouldProduceCorrectExtent

	| rec1 rec2 |
	rec1 := 0@0 extent: 100@100.
	rec2 := rec1 insetBy: 40@40.
	
	self assert: rec2 height = 20. 
	self assert: rec2 width = 20. 