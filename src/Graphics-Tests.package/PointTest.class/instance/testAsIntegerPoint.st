tests - converting
testAsIntegerPoint
	|testPoint|
	self assert: (1 @ 1) asIntegerPoint = (1 @ 1).
	self assert: (1.2 @ 1) asIntegerPoint = (1 @ 1).
	self assert: (1 @ 1.2) asIntegerPoint = (1 @ 1).
	self assert: (1.2 @ 1.2) asIntegerPoint = (1 @ 1).
	self assert: (1.000000000000000001 @ 1.9999999999) asIntegerPoint = (1 @ 1).
	
	self assert: (-1 @ -1) asIntegerPoint = (-1 @ -1).
	self assert: (-1.2 @ -1) asIntegerPoint = (-1 @ -1).
	self assert: (-1 @ -1.2) asIntegerPoint = (-1 @ -1).
	self assert: (-1.2 @ -1.2) asIntegerPoint = (-1 @ -1).
	self assert: (-1.000000000000000001 @ -1.9999999999) asIntegerPoint = (-1 @ -1).
	
	self deny: (1.2 @ 1) asIntegerPoint = (1.2 @ 1).
	self deny: (1 @ 1.2) asIntegerPoint = (1 @ 1.2).
	self deny: (1.2 @ 1.2) asIntegerPoint = (1.2 @ 1.2).
	
	self deny: (-1.2 @ -1) asIntegerPoint = (-1.2 @ -1).
	self deny: (-1 @ -1.2) asIntegerPoint = (-1 @ -1.2).
	self deny: (-1.2 @ -1.2) asIntegerPoint = (-1.2 @ -1.2).
	
	"test that we don't create a new instance if point is already a n integer point"
	testPoint := 1 @ 1.
	self assert: testPoint == testPoint asIntegerPoint.
	"test that we actually create a new instance if point is not an integer one"
	testPoint := 1.0 @ 1.0.
	self deny: testPoint == testPoint asIntegerPoint