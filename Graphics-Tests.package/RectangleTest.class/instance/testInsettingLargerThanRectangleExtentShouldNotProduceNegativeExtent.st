tests
testInsettingLargerThanRectangleExtentShouldNotProduceNegativeExtent

	| rec1 rec2 |
	rec1 := 0@0 extent: 100@100.
	rec2 := rec1 insetBy: 200@200.
	
	self assert: rec2 height >= 0. 
	self assert: rec2 width >= 0. 
	
	rec1 := 0@0 extent: 100@100.
	rec2 := rec1 insetBy: 50@50.
	"because insetBy: reduces from both sides"
	self assert: rec2 height = 0. 
	self assert: rec2 width = 0. 