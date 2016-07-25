initialization
seed: aNumber
	"Refer #privateNextSeed and [1], seed should be positive"
	| newSeed |
	newSeed := aNumber.
	[ newSeed > 0 ] whileFalse: 
		[ newSeed := newSeed + m ].
	seed := newSeed.
