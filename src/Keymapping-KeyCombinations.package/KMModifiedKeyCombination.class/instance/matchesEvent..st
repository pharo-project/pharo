matching
matchesEvent: aKeyboardEvent

	^ (self modifier matchesEvent: aKeyboardEvent) 
		and: [self character matchesEvent: aKeyboardEvent]