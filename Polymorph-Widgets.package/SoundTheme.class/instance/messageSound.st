sounds
messageSound
	"Answer the message sound."

	^self sounds at: #message ifAbsent: [self defaultSound]