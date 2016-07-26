tests - zero behavior
testZeroSignificandAsInteger
	"This is about http://bugs.squeak.org/view.php?id=6990"
	
	self assert: 0.0 significandAsInteger = 0