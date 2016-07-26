accessing
currentPosition: aPoint
	self position: aPoint x @ (aPoint y - self height).
