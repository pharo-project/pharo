event-mouse
mouseUp: evt
	super mouseUp: evt.
	
	pagerModel recomputeState: isResizing not.
	
	isResizing ifTrue: [ self setWidthAfterResizing ].
	isResizing := false.
		
	(self globalBounds containsPoint: evt position)
		ifFalse: [ self setCursorToNormal: evt ]
		ifTrue: [ self setCursorToResizing: evt ] 