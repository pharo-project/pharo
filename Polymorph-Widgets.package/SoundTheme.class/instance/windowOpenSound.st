sounds
windowOpenSound
	"Answer the window open sound."

	^self sounds at: #windowOpen ifAbsent: [self defaultSound]