tests
testPoint
	self assert: (self materialize: 'Point[1,2]') = (1@2)