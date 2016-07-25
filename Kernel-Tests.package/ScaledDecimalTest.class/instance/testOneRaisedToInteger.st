tests
testOneRaisedToInteger
	"One might be handled specially"
	
	self assert: (1.0s1 raisedToInteger: -1) scale = 1.
	self assert: (1.0s1 raisedToInteger: -1) = 1.
	self assert: (1.0s1 raisedToInteger: 0) scale = 1.
	self assert: (1.0s1 raisedToInteger: 0) = 1.
	self assert: (1.0s1 raisedToInteger: 1) scale = 1.
	self assert: (1.0s1 raisedToInteger: 1) = 1.
	self assert: (1.0s1 raisedToInteger: 2) scale = 1.
	self assert: (1.0s1 raisedToInteger: 2) = 1.