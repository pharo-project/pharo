tests - conversions
testAsSmallerPowerOfTwo

"SmallIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/2) asSmallerPowerOfTwo equals: 1/2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 2 denominator:  4) asSmallerPowerOfTwo equals: 1/2.
	"Not power of two"
	self assert: (2 / 5) asSmallerPowerOfTwo equals: 1/4.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 10 denominator: 25) asSmallerPowerOfTwo equals: 1/4.
"SmallIntegers, Fraction value  > 1"
	"Exact power of two"
	self assert: (Fraction numerator: 2 denominator:  1) asSmallerPowerOfTwo equals: 2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 4 denominator:  2) asSmallerPowerOfTwo equals: 2.
	"Not power of two"
	self assert: (11 / 2) asSmallerPowerOfTwo equals: 4.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 44 denominator: 8) asSmallerPowerOfTwo equals: 4.
"LargeIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/(2 raisedTo:80)) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160)) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Not power of two"
	self assert: (1/((2 raisedTo:80) - 1)) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) - 1) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
"LargeIntegers, Fraction value > 1"
	"Exact power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) denominator:  1) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) denominator: (2 raisedTo: 80)) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	"Not power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) +1 denominator:  1) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) +1 denominator: (2 raisedTo: 80)) asSmallerPowerOfTwo equals: (2 raisedTo: 80).

	
"Error condition: numerator is 0"
	self should: [(Fraction numerator: 0 denominator: 1) asSmallerPowerOfTwo] raise: DomainError.
"Errro condition: Negative fraction"
	"SmallIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: 2) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator: 2 negated) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 negated denominator: 4) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 denominator: 4 negated) asSmallerPowerOfTwo] raise: DomainError.
	"LargeNegativeIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: (2 raisedTo: 80)) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator:(2 raisedTo: 80) negated) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) negated denominator: (2 raisedTo: 160)) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) negated) asSmallerPowerOfTwo] raise: DomainError
