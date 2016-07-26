tests - mathematical functions
testRaisedToErrorConditions
	"
	IntegerTest new testRaisedToErrorConditions
	"

	self should: [ -2 raisedTo: 1/4 ] raise: ArithmeticError.
	self should: [ -2 raisedTo: 1.24 ] raise: ArithmeticError.