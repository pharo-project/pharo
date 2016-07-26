tests - mathematical functions
testSqrtErrorConditions
	"
	IntegerTest new testSqrtErrorConditions
	"

	self should: [ -1 sqrt ] raise: ArithmeticError