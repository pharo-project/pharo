tests - infinity behavior
testHugeIntegerCloseTo
	"This is a test for bug http://bugs.squeak.org/view.php?id=7368"
	
 	"FloatTest new testHugeIntegerCloseTo"

	self deny: (1.0 closeTo: 200 factorial).
	self deny: (200 factorial closeTo: 1.0).
	self assert: (Float infinity closeTo: 200 factorial) = (200 factorial closeTo: Float infinity).