tests - mathematical functions
testExactSqrt
	"
	IntegerTest new testExactSqrt
	"
	self assert: 4 sqrt classAndValueEquals: 2.
	self assert: 9 sqrt classAndValueEquals: 3.
	self assert: Float maxExactInteger squared sqrt classAndValueEquals: Float maxExactInteger.
	self assert: (Float maxExactInteger+1) squared sqrt classAndValueEquals: Float maxExactInteger+1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: i squared sqrt classAndValueEquals: i ]