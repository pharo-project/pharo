sounds
windowRestoreDownSound
	"Answer the window restore down sound."

	^self sounds at: #windowRestoreDown ifAbsent: [self defaultSound]