tests
testRaisedToInteger
	"Raising to integer should preserve class and scale"
	
	self assert: (3.0s1 raisedToInteger: -1) scale = 1.
	self assert: (3.0s1 raisedToInteger: -1) = (1/3).
	self assert: (3.0s1 raisedToInteger: 0) scale = 1.
	self assert: (3.0s1 raisedToInteger: 0) = 1.
	self assert: (3.0s1 raisedToInteger: 1) scale = 1.
	self assert: (3.0s1 raisedToInteger: 1) = 3.
	self assert: (3.0s1 raisedToInteger: 2) scale = 1.
	self assert: (3.0s1 raisedToInteger: 2) = 9.