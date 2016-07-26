sounds
windowMinimizeSound
	"Answer the window minimize sound."

	^self sounds at: #windowMinimize ifAbsent: [self defaultSound]