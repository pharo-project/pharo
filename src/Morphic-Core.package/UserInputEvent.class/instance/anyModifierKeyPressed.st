modifier state
anyModifierKeyPressed
	"ignore, however, the shift keys 'cause that's not REALLY a command key "

	^ self buttons anyMask: 2r01110000	"cmd | opt | ctrl"