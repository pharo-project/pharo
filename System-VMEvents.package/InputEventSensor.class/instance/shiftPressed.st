modifier keys
shiftPressed
	"Answer whether the shift key on the keyboard is being held down."

	^self modifiers anyMask: 16r01
