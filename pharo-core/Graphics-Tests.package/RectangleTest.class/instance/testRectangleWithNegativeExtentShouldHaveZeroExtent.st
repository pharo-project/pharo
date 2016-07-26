tests
testRectangleWithNegativeExtentShouldHaveZeroExtent
	
	self assert: (Rectangle origin: 10@10 extent: -5@ -5) extent = (0@0).
	self assert: (Rectangle origin: 10@10 extent: -5@ -5) origin = (10@10).
		self assert: (Rectangle origin: 10@10 extent: 50@ 60) extent = (50@60).