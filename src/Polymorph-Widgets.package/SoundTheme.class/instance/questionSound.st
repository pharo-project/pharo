sounds
questionSound
	"Answer the question sound."

	^self sounds at: #question ifAbsent: [self defaultSound]