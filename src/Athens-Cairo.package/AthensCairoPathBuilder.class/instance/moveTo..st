path commands
moveTo: aPoint
	
	endPoint := self toAbsolute: aPoint.
	lastControlPoint := nil.
	
	^ self moveToX: endPoint x asFloat Y: endPoint y asFloat 