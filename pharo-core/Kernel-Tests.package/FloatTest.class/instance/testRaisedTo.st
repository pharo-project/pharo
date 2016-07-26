tests - mathematical functions
testRaisedTo
	"
	FloatTest new testRaisedTo
	"
	self should: [ -1.23 raisedTo: 1/4 ] raise: ArithmeticError