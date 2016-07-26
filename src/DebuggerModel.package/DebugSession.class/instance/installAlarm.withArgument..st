private
installAlarm: aSelector withArgument: args
	
	World
		addAlarm: aSelector
		withArguments: args
		for: self
		at: Time millisecondClockValue + 200