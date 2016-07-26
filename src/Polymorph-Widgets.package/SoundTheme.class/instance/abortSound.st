sounds
abortSound
	"Answer the abort sound."

	^self sounds at: #abort ifAbsent: [self defaultSound]