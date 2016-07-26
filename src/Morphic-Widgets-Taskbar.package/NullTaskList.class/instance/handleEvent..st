event handling
handleEvent: aKeyboardEvent 

	aKeyboardEvent isWindowNavigation ifFalse: [ ^ false ].
	world theme openTaskListIn: world from: aKeyboardEvent.
	^ true.