tests - conversions
testAsLargerPowerOfTwo

"SmallIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/2) asLargerPowerOfTwo equals: 1/2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 2 denominator:  4) asLargerPowerOfTwo equals: 1/2.
	"Not power of two"
	self assert: (2 / 5) asLargerPowerOfTwo equals: 1/2.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 10 denominator: 25) asLargerPowerOfTwo equals: 1/2.
"SmallIntegers, Fraction value  > 1"
	"Exact power of two"
	self assert: (Fraction numerator: 2 denominator:  1) asLargerPowerOfTwo equals: 2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 4 denominator:  2) asLargerPowerOfTwo equals: 2.
	"Not power of two"
	self assert: (3 / 2) asLargerPowerOfTwo equals: 2.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 12 denominator: 8) asLargerPowerOfTwo equals: 2.
"LargeIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/(2 raisedTo:80)) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160)) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Not power of two"
	self assert: (1/((2 raisedTo: 80)+1)) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) + 1) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
"LargeIntegers, Fraction value > 1"
	"Exact power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) denominator:  1) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) denominator: (2 raisedTo: 80)) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	"Not power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) -1 denominator:  1) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) -1 denominator: (2 raisedTo: 80)) asLargerPowerOfTwo equals: (2 raisedTo: 80).

	
"Error condition: numerator is 0"
	self should: [(Fraction numerator: 0 denominator: 1) asLargerPowerOfTwo] raise: DomainError.
"Errro condition: Negative fraction"
	"SmallIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: 2) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator: 2 negated) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 negated denominator: 4) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 denominator: 4 negated) asLargerPowerOfTwo] raise: DomainError.
	"LargeNegativeIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: (2 raisedTo: 80)) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator:(2 raisedTo: 80) negated) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) negated denominator: (2 raisedTo: 160)) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) negated) asLargerPowerOfTwo] raise: DomainError
	
