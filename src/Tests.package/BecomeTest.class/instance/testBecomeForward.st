testing
testBecomeForward
	"Test the forward become."
	| a b c d |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.

	a becomeForward: b.

	self 
		assert: a = 'cd';
		assert: b = 'cd';
		assert: c = 'cd';
		assert: d = 'cd'.


