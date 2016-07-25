instance creation
send: selector toSuperOf: behavior

	behavior ifNil: [self error: 'super of nil does not exist'].
	^ IRSend new
		selector: selector;
		superOf: behavior;
		yourself.