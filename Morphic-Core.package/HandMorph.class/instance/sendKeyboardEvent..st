private events
sendKeyboardEvent: anEvent 
	"Send the event to the morph currently holding the focus, or if none to
	the owner of the hand."
	^ self
		sendEvent: anEvent
		focus: self keyboardFocus
		clear: [self keyboardFocus: nil]