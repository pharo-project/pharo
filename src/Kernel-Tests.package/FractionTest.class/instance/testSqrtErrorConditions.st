tests - mathematical functions
testSqrtErrorConditions
	"
	FractionTest new testSqrtErrorConditions
	"
	self should: [ (-1/4) sqrt ] raise: DomainError.
	self should: [ ((1 << 1024 + 1) / (1 << 1024 + 3)) negated sqrt ] raise: DomainError