tests - point functions
testDistanceTo

	self assert: (0 @ 0 distanceTo: 0 @ 0) = 0.
	self assert: (0 @ 0 distanceTo: 0 @ 5) = 5.
	self assert: (5 @ 0 distanceTo: 0 @ 0) = 5.
	self assert: (3 @ 0 distanceTo: 0 @ 4) = 5.
	self assert: (5 @ 0 distanceTo: 2 @ 4) = 5.
	self deny: (0 @ 0 distanceTo: 0 @ 0) = 1.