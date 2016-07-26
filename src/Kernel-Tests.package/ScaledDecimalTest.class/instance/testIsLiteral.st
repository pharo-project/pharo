tests
testIsLiteral
	"This test is related to http://bugs.squeak.org/view.php?id=6796"
	
	self assert: 1.00s2 isLiteral description: 'every literal obviously isLiteral'.
	
	"Note that (1 / 3.00s2) is not a well behaved literal,
	because it does not re-evaluate to self...
	Every literal should be evaluated as self (see isSelfEvaluating).
	There is currently no way to print it as a literal.
	So i propose it shall not answer true."
	self deny: (1/3.00s2) isLiteral description: 'this number cannot represent itself as a literal'.