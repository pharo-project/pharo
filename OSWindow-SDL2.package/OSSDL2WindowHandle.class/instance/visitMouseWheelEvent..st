event handling
visitMouseWheelEvent: sdlEvent
	| osEvent |
	osEvent := OSMouseWheelEvent for: osWindow.
	osEvent position: self mousePosition;
			scrollHorizontal: sdlEvent x;
			scrollVertical: sdlEvent y.
			
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.