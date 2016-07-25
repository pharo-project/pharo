api-shortcuts
removeMenuKeyCombination: aShortcut

	additionalKeyBindings 
		removeKey: aShortcut 
		ifAbsent: [ ^ self ].
	
	self changed: #removeMenuKeyCombination: with: { aShortcut }