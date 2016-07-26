tests
testAt
	self assert: ((1 to: 10)
			at: 1)
			= 1.
	self assert: ((1 to: 10)
			at: 3)
			= 3.
	self assert: ((1 to: 10 by: 2)
			at: 1)
			= 1.
	self assert: ((1 to: 10 by: 2)
			at: 3)
			= 5