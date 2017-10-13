api-shortcuts
removeKeyCombination: aShortcut

	additionalKeyBindings 
		removeKey: aShortcut 
		ifAbsent: [ ^ self ].
	
	self changed: #removeKeyCombination: with: { aShortcut }