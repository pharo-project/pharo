sounds
alertSound
	"Answer the alert sound."

	^self sounds at: #alert ifAbsent: [self defaultSound]