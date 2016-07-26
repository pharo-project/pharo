tests - Class Methods
testDigitAt

	| number |
	number := 16rABCDEF.
	self assert: (number digitAt: 1) equals: 16rEF.
	self assert: (number digitAt: 2) equals: 16rCD.
	self assert: (number digitAt: 3) equals: 16rAB.