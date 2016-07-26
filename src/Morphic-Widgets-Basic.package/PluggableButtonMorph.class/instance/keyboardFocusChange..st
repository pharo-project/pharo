keyboard
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged