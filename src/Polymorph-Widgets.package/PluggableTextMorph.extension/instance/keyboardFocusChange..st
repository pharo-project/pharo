*Polymorph-Widgets
keyboardFocusChange: aBoolean
	"Pass on to text morph."
	super keyboardFocusChange: aBoolean.
	aBoolean ifTrue: [self textMorph takeKeyboardFocus]