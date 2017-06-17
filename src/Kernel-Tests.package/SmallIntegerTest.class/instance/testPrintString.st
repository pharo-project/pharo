tests - printing
testPrintString
	self assert: 1 printString equals: '1'.
	self assert: -1 printString equals: '-1'.
	Smalltalk vm wordSize = 4
		ifTrue: [ 
			self assert: SmallInteger minVal printString equals: '-1073741824'.
			self assert: SmallInteger maxVal printString equals: '1073741823' ].
	Smalltalk vm wordSize = 8
		ifTrue: [ 
			self assert: SmallInteger minVal printString equals: '-1152921504606846976'.
			self assert: SmallInteger maxVal printString equals: '1152921504606846975' ].
	self assert: 12345 printString equals: '12345'.
	self assert: -54321 printString equals: '-54321'.
	
	self assert: 0 decimalDigitLength equals: 1.
	self assert: 4 decimalDigitLength equals: 1.
	self assert: 12 decimalDigitLength equals: 2.
	self assert: 123 decimalDigitLength equals: 3.
	self assert: 1234 decimalDigitLength equals: 4.
	self assert: 56789 decimalDigitLength equals: 5.
	self assert: 657483 decimalDigitLength equals: 6.
	self assert: 6571483 decimalDigitLength equals: 7.
	self assert: 65174383 decimalDigitLength equals: 8.
	self assert: 625744831 decimalDigitLength equals: 9.
	self assert: 1000001111 decimalDigitLength equals: 10.
	Smalltalk vm wordSize = 4
		ifTrue: [ self assert: SmallInteger maxVal decimalDigitLength equals: 10 ].
	Smalltalk vm wordSize = 8
		ifTrue: [ self assert: SmallInteger maxVal decimalDigitLength equals: 19 ].