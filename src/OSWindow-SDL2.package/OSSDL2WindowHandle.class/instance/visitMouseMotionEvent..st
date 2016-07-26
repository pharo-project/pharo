event handling
visitMouseMotionEvent: sdlEvent
	| osEvent |
	osEvent := OSMouseMoveEvent for: osWindow.
	osEvent position: sdlEvent x @ sdlEvent y;
		delta: sdlEvent xrel @ sdlEvent yrel.

	self convertButtonState: sdlEvent state modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.