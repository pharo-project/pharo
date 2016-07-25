private events
sendEvent: anEvent focus: focusHolder
	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."
	^self sendEvent: anEvent focus: focusHolder clear:[nil]