tests - zero behavior
testZero1
	"FloatTest new testZero1"

	self assert: Float negativeZero = 0 asFloat.
	self assert: (Float negativeZero at: 1) ~= (0 asFloat at: 1).

	"The negative zero has a bit representation that is different from the bit representation of the positive zero. Nevertheless, both values are defined to be equal."
