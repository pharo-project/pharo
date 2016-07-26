encoding
encodeLiteral: lit
	"Encode the given literal"
	litCount := litCount + 1.
	literals at: litCount put: lit.
	distances at: litCount put: 0.
	literalFreq at: lit+1 put: (literalFreq at: lit+1) + 1.
	^self shouldFlush