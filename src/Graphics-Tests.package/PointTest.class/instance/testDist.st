tests - point functions
testDist

	self assert: (0 @ 0 dist: 0 @ 0) = 0.
	self assert: (0 @ 0 dist: 0 @ 5) = 5.
	self assert: (5 @ 0 dist: 0 @ 0) = 5.
	self assert: (3 @ 0 dist: 0 @ 4) = 5.
	self assert: (5 @ 0 dist: 2 @ 4) = 5.
	self deny: (0 @ 0 dist: 0 @ 0) = 1.