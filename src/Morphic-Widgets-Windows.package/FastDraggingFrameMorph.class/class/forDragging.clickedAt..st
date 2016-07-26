instance creation
forDragging: aWindow clickedAt: aPoint
	| b |
	b := self new.
	b target: aWindow.
	b startGap: aWindow topLeft - aPoint.
	^ b