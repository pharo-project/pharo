changing state
tryToGoThroughNextState
	modeActivated
		ifTrue: [ self tryToEndGesture ]
		ifFalse: [ self tryToStartGesture ]