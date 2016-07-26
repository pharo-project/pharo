bit manipulation
bitAt: anInteger
	"Answer 1 if the bit at position anInteger is set to 1, 0 otherwise.
	self is considered an infinite sequence of bits, so anInteger can be any strictly positive integer.
	Bit at position 1 is the least significant bit.
	Negative numbers are in two-complements.
	
	This is a naive implementation that can be refined in subclass for speed"
	
	^(self bitShift: 1 - anInteger) bitAnd: 1