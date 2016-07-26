modifier keys
anyModifierKeyPressed
	"ignore, however, the shift keys 'cause that's not REALLY a command key"

	^self modifiers anyMask: 16r0E	"cmd | opt | ctrl"