event handling
visitMouseButtonDownEvent: event
	| osEvent |
	osEvent := OSMouseButtonPressEvent for: osWindow.
	osEvent button: event button;
			position: event x @ event y.
	
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.