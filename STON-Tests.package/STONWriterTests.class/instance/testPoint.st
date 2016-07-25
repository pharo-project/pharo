tests
testPoint
	self assert: (self serialize: 1@2) = 'Point[1,2]'