tests - basic
testAsSmallerPowerOfTwo
"Invalid input testing"
	"LargeNegativeIntegers"
	self should: [(2 raisedTo: 80) negated asSmallerPowerOfTwo] raise: DomainError.
	"Negative SmallIntegers"
	self should: [-1 asSmallerPowerOfTwo] raise: DomainError.
	"0"
	self should: [0 asSmallerPowerOfTwo] raise: DomainError.
"Valid inputs"
	"Small integers"
	self assert: 1 asSmallerPowerOfTwo equals: 1.
	self assert: 2 asSmallerPowerOfTwo equals: 2.
	self assert: 3 asSmallerPowerOfTwo equals: 2.
	self assert: 4 asSmallerPowerOfTwo equals: 4.
	self assert: 5 asSmallerPowerOfTwo equals: 4.
	"Large integers"
	self assert: ((2 raisedTo: 80) +1) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	self assert: (2 raisedTo: 80) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	self assert: ((2 raisedTo: 80) - 1) asSmallerPowerOfTwo equals: (2 raisedTo: 80 - 1)