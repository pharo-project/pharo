visiting
visitMouseButtonPressEvent: anEvent
	anEvent button = 1
		ifTrue: [ ^ athensExample startStrokeAt: anEvent position ].
	athensExample clearStrokes