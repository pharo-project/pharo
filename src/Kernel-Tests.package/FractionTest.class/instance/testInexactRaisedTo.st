tests - mathematical functions
testInexactRaisedTo
	"
	FractionTest new testInexactRaisedTo
	"
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) raisedTo: 1/3) = 1.0.
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/3) = -1.0