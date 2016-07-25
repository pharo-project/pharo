calculating
isGettingCloserToPoint: aPoint
	| rectangle |
	rectangle := lastPosition rectangle: aPoint.
	^ (rectangle containsPoint: lastPosition + delta)