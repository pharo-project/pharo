recategorized
rejectsEvent: anEvent
	"Return true to reject the given event.
	Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it.
	If the event is a mouse wheel event then only reject if the receiver is not visible."
	
	(anEvent isMouse and: [anEvent isMouseWheel])
		ifTrue: [^self visible not].
	^self isLocked or: [self visible not]