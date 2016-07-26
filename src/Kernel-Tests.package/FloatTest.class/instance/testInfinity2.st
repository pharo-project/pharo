tests - infinity behavior
testInfinity2
   "FloatTest new testInfinity2"

	| i1  i2 |
	i1 := 10000 exp.
	i2 := 1000000000 exp.
	i2 := 0 - i2. " this is entirely ok. You can compute with infinite values."

	self assert: i1 isInfinite & i2 isInfinite & i1 positive & i2 negative.
	self deny: i1 = i2.
  	"All infinities are signed. Negative infinity is not equal to Infinity"
