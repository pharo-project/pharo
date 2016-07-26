sounds
windowCloseSound
	"Answer the window close sound."

	^self sounds at: #windowClose ifAbsent: [self defaultSound]