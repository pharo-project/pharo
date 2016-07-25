arithmetic
reciprocalModulo: n
	"Answer an integer x such that (self * x) \\ n = 1, x > 0, x < n.
	Raise an error if there is no such integer.
	The algorithm is a non extended euclidean modular inversion called NINV.
	It is described in this article:
		'Using an RSA Accelerator for Modular Inversion'
	by Martin Seysen. See http://www.iacr.org/archive/ches2005/017.pdf"

	| u v f fPlusN b result result2 |
	((self <= 0) or: [n <= 0]) ifTrue: [self error: 'self and n must be greater than zero'].
	self >= n ifTrue: [self error: 'self must be < n'].

	b := n highBit + 1.
	f := 1 bitShift: b.
	v := (self bitShift: b) + 1.
	u := n bitShift: b.
	fPlusN := f + n.
	[v >= fPlusN] whileTrue:
		[v := u \\ (u := v)].
	result := v - f.
	(result2 := result + n) > 0
		ifFalse: [self error: 'no inverse'].
	^result positive
		ifTrue: [result]
		ifFalse: [result2]