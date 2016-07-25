testing
testBecomeForwardCopyIdentityHash
	"Check that
		1. the argument to becomeForward: IS modified to have the sender's identity hash.
		2. the sender's identity hash is unchanged."

 	| a b identityHashOfA |

	a := 'ab' copy.
	b := 'cd' copy.
	identityHashOfA := a identityHash.

	a becomeForward: b copyHash: true.

	self 
		assert: a == b;
		assert: a identityHash = identityHashOfA;
		assert: b identityHash = identityHashOfA.

