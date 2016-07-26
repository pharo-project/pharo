sounds
denySound
	"Answer the deny sound."

	^self sounds at: #deny ifAbsent: [self defaultSound]