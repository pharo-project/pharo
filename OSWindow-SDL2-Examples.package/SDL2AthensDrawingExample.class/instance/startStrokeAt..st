managing strokes
startStrokeAt: aPoint
	currentStroke := OrderedCollection with: aPoint.
	strokes add: currentStroke