utils
installAlarm: aSelector for: anObject

	World
		addAlarm: aSelector
		withArguments: #()
		for: anObject
		at: Time millisecondClockValue + 50.