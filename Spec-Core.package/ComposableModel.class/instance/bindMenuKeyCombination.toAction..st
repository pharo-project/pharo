api-shortcuts
bindMenuKeyCombination: aShortcut toAction: aBlock

	additionalKeyBindings at: aShortcut put: aBlock.
	
	self changed: #bindMenuKeyCombination:toAction: with: {aShortcut . aBlock}