event handling
keyStroke: evt
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	self eventHandler ifNotNil:
		[self eventHandler keyStroke: evt fromMorph: self].
