event handling
mouseDown: event 
	super mouseDown: event.
	event yellowButtonPressed 
		ifTrue: [ self showContextMenu ]
		ifFalse: [ World activeHand newKeyboardFocus: self ]