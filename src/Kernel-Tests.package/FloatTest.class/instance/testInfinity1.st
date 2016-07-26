tests - infinity behavior
testInfinity1
   "FloatTest new testInfinity1"

	| i1  i2 |

	i1 := 10000 exp.
	i2 := 1000000000 exp.
	self assert: i1 isInfinite & i2 isInfinite & (i1 = i2).
	"All infinities are equal. (This is a very substantial difference to NaN's, which are never equal."
