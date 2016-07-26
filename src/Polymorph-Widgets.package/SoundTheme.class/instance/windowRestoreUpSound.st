sounds
windowRestoreUpSound
	"Answer the window restore up sound."

	^self sounds at: #windowRestoreUp ifAbsent: [self defaultSound]