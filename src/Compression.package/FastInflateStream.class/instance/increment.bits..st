huffman trees
increment: value bits: nBits
	"Increment value in reverse bit order, e.g. 
	for a 3 bit value count as follows:
		000 / 100 / 010 / 110
		001 / 101 / 011 / 111
	See the class comment why we need this."
	| result bit |
	result := value.
	"Test the lowest bit first"
	bit := 1 << (nBits - 1).
	"If the currently tested bit is set then we need to
	turn this bit off and test the next bit right to it"
	[(result bitAnd: bit) = 0] whileFalse:[ 
		"Turn off current bit"
		result := result bitXor: bit.
		"And continue testing the next bit"
		bit := bit bitShift: -1].
	"Turn on the right-most bit that we haven't touched in the loop above"
	^result bitXor: bit