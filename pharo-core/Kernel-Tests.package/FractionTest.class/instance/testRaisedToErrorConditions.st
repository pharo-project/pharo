tests - mathematical functions
testRaisedToErrorConditions
	"
	FractionTest new testRaisedToErrorConditions
	"
	self should: [ (-1/16) raisedTo: 1/4 ] raise: ArithmeticError.
	self should: [ ((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/4 ] raise: ArithmeticError