event handling
mouseMove: evt
	self isForDragging 
		ifTrue: [self draggedTo: evt position]
		ifFalse: [self reframedTo: evt position]
