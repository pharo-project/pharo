event handling
handleMouseDownOnSlider: evt mappedPosition: aNumber 
	| originalPos |
	
	originalPos := self originForDragStart: evt position.
	captured := true. 
	self changed.
	
	[ 
		evt hand captureEventsWhile: [ :event |
			event isMouse not or: [ self handleEventWhileDraggingSlider: event delta: originalPos ]
		].
	] ensure: [  
		captured := false. 
		self changed.
		self currentHand releaseMouseFocus
	 ]