tests - conversion
testAsLayoutFrame
	| frame |
	
	frame := #(0.66 0.5 1 0.77 1 2 3 25) asLayoutFrame.
	self assert: frame leftFraction equals: 0.66.
	self assert: frame topFraction equals: 0.5.
	self assert: frame leftOffset equals: 1.
	self assert: frame topOffset equals: 2.
	
	self assert: frame rightFraction equals: 1.
	self assert: frame bottomFraction equals: 0.77.
	self assert: frame rightOffset equals: 3.
	self assert: frame bottomOffset equals: 25.