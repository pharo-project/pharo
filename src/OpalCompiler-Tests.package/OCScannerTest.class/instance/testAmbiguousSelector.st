testing
testAmbiguousSelector
	"Non regression test.
	See http://code.google.com/p/pharo/issues/detail?id=2271
	and http://bugs.squeak.org/view.php?id=7491"

	'1@-1' parseLiterals.
	self assert: ('1@-1' parseLiterals at: 2) asString = '@-'