event handling
mouseDown: event 
	super mouseDown: event.
	World activeHand newKeyboardFocus: self