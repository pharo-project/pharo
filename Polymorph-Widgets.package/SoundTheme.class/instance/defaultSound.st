sounds
defaultSound
	"Answer the default sound."

	^self sounds at: #default ifAbsent: [self defaultDefaultSound]