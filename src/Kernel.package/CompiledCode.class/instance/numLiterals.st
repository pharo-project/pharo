accessing
numLiterals
	"Answer the number of literals used by the receiver."
	^self header bitAnd: 16r7FFF