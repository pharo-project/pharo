testing
testBecomeHash

	| a b c d ha hb |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.
	ha := a hash.
	hb := b hash.

	a become: b.

	self 
		assert: a hash = hb;
		assert: b hash = ha;
		assert: c hash = hb;
		assert: d hash = ha.


