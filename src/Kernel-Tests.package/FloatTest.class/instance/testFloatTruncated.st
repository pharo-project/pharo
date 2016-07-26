tests - conversion
testFloatTruncated
	"(10 raisedTo: 16) asFloat has an exact representation (no round off error).
	It should convert back to integer without loosing bits.
	This is a no regression test on http://bugs.impara.de/view.php?id=3504"
	
	| x y int r |
	int := 10 raisedTo: 16.
	x := int asFloat.
	y := (5 raisedTo: 16) asFloat timesTwoPower: 16.
	self assert: x = y.
	
	self assert: x asInteger = int.
	
	"this one should be true for any float"
	self assert: x asInteger = x asTrueFraction asInteger.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x truncated = x asTrueFraction truncated]