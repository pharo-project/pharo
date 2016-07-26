tests - basic
testIsPowerOfTwo
	"LargeNegativeIntegers"
	self deny: (2 raisedTo: 80) negated isPowerOfTwo.
	"Negative SmallIntegers"
	self deny: (-1 isPowerOfTwo).
	" 0, incase implementation has forgotten edge case"
	self deny: (0 isPowerOfTwo).
	"Positive SmallIntegers"
	self assert: (1 isPowerOfTwo).
	self assert: (2 isPowerOfTwo).
	self deny:  (3 isPowerOfTwo).
	self assert: (4 isPowerOfTwo).
	self deny: (5 isPowerOfTwo).
	" LargePositiveIntegers"
	self deny: ((2 raisedTo: 80) - 1) isPowerOfTwo.
	self assert: (2 raisedTo: 80) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) + 1) isPowerOfTwo