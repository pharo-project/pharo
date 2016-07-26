tests - mathematical functions
testInexactSqrt
	"
	FractionTest new testInexactSqrt
	"
	self assert: ((1 << 1024 + 1) / (1 << 1024 + 3)) sqrt = 1.0