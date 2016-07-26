events-processing
containsPoint: aPoint event: anEvent
	"Blue buttons are handled by the halo"
	(anEvent isMouse and:[anEvent isMouseDown and:[anEvent blueButtonPressed]])
		ifFalse:[^super containsPoint: aPoint event: anEvent].
	^bounds containsPoint: anEvent position