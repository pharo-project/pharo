event handling
preferredKeyboardPosition

	| pos |
	pos := super preferredKeyboardPosition.
	^ pos + (self bounds: self bounds in: World) topLeft.
