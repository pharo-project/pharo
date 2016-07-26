utils
installAlarm: aSelector for: anObject withArguments: anArray

	World
		addAlarm: aSelector
		withArguments: anArray
		for: anObject
		at: Time millisecondClockValue + 50.