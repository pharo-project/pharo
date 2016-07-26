tests
testInteger
	self assert: (self materialize: '1') = 1.
	self assert: (self materialize: '-1') = -1.
	self assert: (self materialize: '0') = 0.
	self assert: (self materialize: '1234567890') = 1234567890.
	self assert: (self materialize: '-1234567890') = -1234567890