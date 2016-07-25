sounds
windowMaximizeSound
	"Answer the window maximize sound."

	^self sounds at: #windowMaximize ifAbsent: [self defaultSound]