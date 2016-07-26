tests - infinity behavior
testInfinityCloseTo
	"This is a test for bug http://bugs.squeak.org/view.php?id=6729:"
	
 	"FloatTest new testInfinityCloseTo"

	self deny: (Float infinity closeTo: Float infinity negated).
	self deny: (Float infinity negated closeTo: Float infinity).