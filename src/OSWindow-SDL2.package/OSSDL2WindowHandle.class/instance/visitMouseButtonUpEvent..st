event handling
visitMouseButtonUpEvent: event
	| osEvent |
	osEvent := OSMouseButtonReleaseEvent for: osWindow.
	osEvent button: event button;
			position: event x @ event y.
	
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.