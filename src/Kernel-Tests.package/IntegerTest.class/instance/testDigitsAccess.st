tests - basic
testDigitsAccess

	self assert: (42 digitAt: 2 base: 10) equals: 4.
	self assert: (42 digitAt: 1 base: 10) equals: 2.
	self assert: (1 digitAt: 2 base: 10) equals: 0.
	self assert: (16rFF digitAt: 1 base: 16) equals: 15.

	self assert: (42 decimalDigitAt: 2) equals: 4.
	self assert: (42 decimalDigitAt: 1) equals: 2.
	
	self assert: (42 decimalDigitLength) equals: 2.
	self assert: (0 decimalDigitLength) equals: 1.
	self assert: (100000000000 decimalDigitLength) equals: 12. "large integer"

	"last digit of the integer base 256"
	self assert: (255 lastDigit) equals: 255.
	self assert: (256 lastDigit) equals: 1.
	self assert: ((256*256-1) lastDigit) equals: 255.
	self assert: ((256*256) lastDigit) equals: 1.

	