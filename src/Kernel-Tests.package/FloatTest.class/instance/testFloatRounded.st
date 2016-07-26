tests - conversion
testFloatRounded
	"5000000000000001 asFloat has an exact representation (no round off error).
	It should round to nearest integer without loosing bits.
	This is a no regression test on http://bugs.squeak.org/view.php?id=7134"
	
	| x y int r |
	
	"This is a preamble asserting exactness of representation
	and quality of various conversions"
	int := 5000000000000001.
	x := int asFloat.
	y := (5 asFloat squared squared squared squared timesTwoPower: 15) + 1.
	self assert: x = y.
	self assert: x asTrueFraction = int.
	
	"this one should be true for any float
	in order to conform to ISO/IEC 10967-2"
	self assert: x rounded = x asTrueFraction rounded.
	self assert: x negated rounded = x negated asTrueFraction rounded.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x rounded = x asTrueFraction rounded.
		self assert: x negated rounded = x negated asTrueFraction rounded]