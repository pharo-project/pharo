path commands
lineTo: aPoint

	endPoint := self toAbsolute: aPoint.
	lastControlPoint := nil.
	^ self lineToX: endPoint x asFloat Y: endPoint y asFloat