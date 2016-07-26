testing
testBecomeForwardDontCopyIdentityHash
	"Check that
		1. the argument to becomeForward: is NOT modified to have the receiver's identity hash.
		2. the receiver's identity hash is unchanged."

 	| a b identityHashOfB |

	a := 'ab' copy.
	b := 'cd' copy.
	identityHashOfB := b identityHash.

	a becomeForward: b copyHash: false.

	self 
		assert: a == b;
		assert: a identityHash = identityHashOfB;
		assert: b identityHash = identityHashOfB.

