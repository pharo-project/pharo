tests - Float
testIsNumber

	self assert: (NumberParser isNumber: '-1.2') equals: true.
	self assert: (NumberParser isNumber: '2e-2') equals: true.
	
	self assert: (NumberParser isNumber: '') equals: false.
	self assert: (NumberParser isNumber: '2a') equals: false.
	self assert: (NumberParser isNumber: '--1') equals: false.
	self assert: (NumberParser isNumber: '1-') equals: false.
	self assert: (NumberParser isNumber: '1..2') equals: false.