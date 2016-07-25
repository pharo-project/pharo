tests
testRaisedTo
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"
	
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.