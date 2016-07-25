changing state
tryToEndGesture
	self stayActiveConditions
		ifTrue: [ ^ self ].
	modeActivated := false.
	self sendToEventHandler: self createEventEnding