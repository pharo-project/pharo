testing
isNavigationEvent: aKeyboardEvent

	^ aKeyboardEvent asKeyCombination = Character tab alt or: [ aKeyboardEvent asKeyCombination = Character tab alt shift ].