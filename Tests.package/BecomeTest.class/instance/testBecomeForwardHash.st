testing
testBecomeForwardHash

	| a b c hb |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	hb := b hash.

	a becomeForward: b.

	self 
		assert: a hash = hb;
		assert: b hash = hb;
		assert: c hash = hb.


