tests - conversion
testRounded
	self assert: 0.9 rounded = 1.
	self assert: 1.0 rounded = 1.
	self assert: 1.1 rounded = 1.
	self assert: -1.9 rounded = -2.
	self assert: -2.0 rounded = -2.
	self assert: -2.1 rounded = -2.
	
	"In case of tie, round to upper magnitude"
	self assert: 1.5 rounded = 2.
	self assert: -1.5 rounded = -2.