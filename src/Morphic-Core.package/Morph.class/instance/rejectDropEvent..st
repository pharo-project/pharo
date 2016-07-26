events-processing
rejectDropEvent: anEvent
	"This hook allows the receiver to repel a drop operation currently executed. The method is called prior to checking children so the receiver must validate that the event was really designated for it.
	Note that the ordering of the tests below is designed to avoid a (possibly expensive) #fullContainsPoint: test. If the receiver doesn't want to repel the morph anyways we don't need to check after all."
	(self repelsMorph: anEvent contents event: anEvent) ifFalse:[^self]. "not repelled"
	(self fullContainsPoint: anEvent position) ifFalse:[^self]. "not for me"
	"Throw it away"
	anEvent wasHandled: true.
	anEvent contents rejectDropMorphEvent: anEvent.