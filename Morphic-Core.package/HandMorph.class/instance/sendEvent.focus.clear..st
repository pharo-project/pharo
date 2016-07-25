private events
sendEvent: anEvent focus: focusHolder clear: aBlock
	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."
	| result |
	focusHolder ifNotNil:[^self sendFocusEvent: anEvent to: focusHolder clear: aBlock].
	ActiveEvent := anEvent.
	result := owner processEvent: anEvent.
	ActiveEvent := nil.
	^result