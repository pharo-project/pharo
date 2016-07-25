private events
sendMouseEvent: anEvent
	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."
	^self sendEvent: anEvent focus: self mouseFocus clear:[self mouseFocus: nil]