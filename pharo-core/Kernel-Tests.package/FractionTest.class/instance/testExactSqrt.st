tests - mathematical functions
testExactSqrt
	"
	FractionTest new testExactSqrt
	"
	| f |
	self assert: (4/9) sqrt classAndValueEquals: 2/3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :i :j |
		f := i / j.
		self assert: f squared sqrt classAndValueEquals: f.
		f := j / i.
		self assert: f squared sqrt classAndValueEquals: f ]