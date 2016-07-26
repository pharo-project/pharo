tests - basic
testIsPowerOfTwoM6873
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6873"

	self deny: ((1 to: 80) anySatisfy: [:n | (2 raisedTo: n) negated isPowerOfTwo])
		description: 'A negative integer cannot be a power of two'.