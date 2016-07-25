tests
testZeroRaisedToInteger
	"Zero might be handle specially"
	
	self should: [0.0s1 raisedToInteger: -1] raise: Error.
	self assert: (0.0s1 raisedToInteger: 0) = 1.
	self assert: (0.0s1 raisedToInteger: 0) scale = 1.
	self assert: (0.0s1 raisedToInteger: 1) = 0.
	self assert: (0.0s1 raisedToInteger: 1) scale = 1.
	self assert: (0.0s1 raisedToInteger: 2) = 0.
	self assert: (0.0s1 raisedToInteger: 2) scale = 1.