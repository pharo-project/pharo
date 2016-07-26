changing state
tryToStartGesture
	self beginConditions ifFalse: [ ^self ].
	modeActivated := true.
	self sendToEventHandler: self createEventBeginning