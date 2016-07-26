matching
matchesEvent: aKeyboardEvent
	^ aKeyboardEvent asKeyCombination modifier = OSPlatform current defaultModifier