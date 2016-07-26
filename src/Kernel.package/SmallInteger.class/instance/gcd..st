arithmetic
gcd: anInteger 
	"See SmallInteger (Integer) | gcd:"
	| n m |
	n := self.
	m := anInteger.
	[n = 0]
		whileFalse: 
			[n := m \\ (m := n)].
	^ m abs