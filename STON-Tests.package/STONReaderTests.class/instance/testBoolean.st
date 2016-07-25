tests
testBoolean
	self assert: (self materialize: 'true') equals: true.
	self assert: (self materialize: 'false') equals: false