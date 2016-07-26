event-mouse
handleMouseOver: anEvent
	super handleMouseOver: anEvent.
		
	isResizing | (self isLeftResizingZone: anEvent) | (self isRightResizingZone: anEvent)
		ifTrue: [ self setCursorToResizing: anEvent ]
		ifFalse: [ self setCursorToNormal: anEvent ].