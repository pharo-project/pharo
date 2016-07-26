tests - basic
testAsLargerPowerOfTwo
"Invalid input testing"
	"LargeNegativeIntegers"
	self should: [(2 raisedTo: 80) negated asLargerPowerOfTwo] raise: DomainError.
	"Negative SmallIntegers"
	self should: [-1 asLargerPowerOfTwo] raise: DomainError.
	"0"
	self should: [0 asLargerPowerOfTwo] raise: DomainError.
"Valid inputs"
	"Small integers"
	self assert: 1 asLargerPowerOfTwo equals: 1.
	self assert: 2 asLargerPowerOfTwo equals: 2.
	self assert: 3 asLargerPowerOfTwo equals: 4.
	self assert: 4 asLargerPowerOfTwo equals: 4.
	self assert: 5 asLargerPowerOfTwo equals: 8.
	"Large integers"
	self assert: ((2 raisedTo: 80) +1) asLargerPowerOfTwo equals: (2 raisedTo: 80 +1).
	self assert: (2 raisedTo: 80) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	self assert: ((2 raisedTo: 80) - 1) asLargerPowerOfTwo equals: (2 raisedTo: 80)