testing
testBecomeForwardIdentityHash
	"Check that
		1. the argument to becomeForward: is modified to have the receiver's identity hash.
		2. the receiver's identity hash is unchanged."

 	| a b ha |

	a := 'ab' copy.
	b := 'cd' copy.
	ha := a identityHash.

	a becomeForward: b.

	self 
		assert: a identityHash = ha;
		assert: b identityHash = ha.

