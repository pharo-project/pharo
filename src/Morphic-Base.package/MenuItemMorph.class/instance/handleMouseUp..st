events
handleMouseUp: anEvent
	"The handling of control between menu item requires them to act on mouse up even if not the current focus. This is different from the default behavior which really only wants to handle mouse ups when they got mouse downs before"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	anEvent hand releaseMouseFocus: self.
	anEvent wasHandled: true.
	anEvent blueButtonChanged
		ifTrue:[self blueButtonUp: anEvent]
		ifFalse:[self mouseUp: anEvent].