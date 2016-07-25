comparing
hash
	"Hash is reimplemented because = is implemented.
	Care is taken that a Fraction equal to a Float also have an equal hash"

	| tmp |
	denominator isPowerOfTwo ifTrue: [
		"If denominator is a power of two, I can be exactly equal to a Float"
		tmp := self asFloat.
		tmp isFinite ifTrue: [^tmp hash]].
	
	"Else, I cannot be exactly equal to a Float, use own hash algorithm.
	(Assume the fraction is already reduced)"
	^numerator hash bitXor: denominator hash