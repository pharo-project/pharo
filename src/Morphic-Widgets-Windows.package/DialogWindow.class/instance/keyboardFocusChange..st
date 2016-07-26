event handling
keyboardFocusChange: aBoolean
	"Set the focus to the default button."
	super keyboardFocusChange: aBoolean.
	aBoolean ifTrue: [
		self defaultFocusMorph ifNotNil: [:b |
			b takeKeyboardFocus]]