testing
testBecomeIdentityHash
	"Note. The identity hash of both objects seems to change after the become:"

	| a b c d |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.

	a become: b.

	self 
		assert: a identityHash = c identityHash;
		assert: b identityHash = d identityHash;
		deny: a identityHash = b identityHash.
