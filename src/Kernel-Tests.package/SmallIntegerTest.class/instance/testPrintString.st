tests - printing
testPrintString
	self assert: 1 printString  = '1'.
	self assert: -1 printString  = '-1'.
	self assert: SmallInteger minVal printString  = '-1073741824'.
	self assert: SmallInteger maxVal printString  = '1073741823'.
	self assert: 12345 printString  = '12345'.
	self assert: -54321 printString  = '-54321'.

	self assert: 0 decimalDigitLength = 1.
	self assert: 4 decimalDigitLength = 1.
	self assert: 12 decimalDigitLength = 2.
	self assert: 123 decimalDigitLength = 3.
	self assert: 1234 decimalDigitLength = 4.
	self assert: 56789 decimalDigitLength = 5.
	self assert: 657483 decimalDigitLength = 6.
	self assert: 6571483 decimalDigitLength = 7.
	self assert: 65174383 decimalDigitLength = 8.
	self assert: 625744831 decimalDigitLength = 9.
	self assert: 1000001111 decimalDigitLength = 10.
	self assert: SmallInteger maxVal decimalDigitLength = 10.