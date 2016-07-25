matching
matchesEvent: aKeyboardEvent
	^ self key asLowercase = aKeyboardEvent modifiedCharacter asLowercase