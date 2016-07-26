tests - conversions
testRounded
	self assert: (4 / 5) rounded = 1.
	self assert: (6 / 5) rounded = 1.
	self assert: (-4 / 5) rounded = -1.
	self assert: (-6 / 5) rounded = -1.
	
	"In case of tie, round to upper magnitude"
	self assert: (3 / 2) rounded = 2.
	self assert: (-3 / 2) rounded = -2.