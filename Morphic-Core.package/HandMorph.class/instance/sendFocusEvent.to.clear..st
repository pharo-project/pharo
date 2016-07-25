private events
sendFocusEvent: anEvent to: focusHolder clear: aBlock
	"Send the event to the morph currently holding the focus"
	| result w |
	w := focusHolder world ifNil:[^ aBlock value].
	w becomeActiveDuring:[
		ActiveHand := self.
		ActiveEvent := anEvent.
		result := focusHolder handleFocusEvent: 
			(anEvent transformedBy: (focusHolder transformedFrom: self)).
	].
	^result