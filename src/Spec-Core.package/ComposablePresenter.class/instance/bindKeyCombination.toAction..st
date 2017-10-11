api-shortcuts
bindKeyCombination: aShortcut toAction: aBlock

	additionalKeyBindings at: aShortcut put: aBlock.
	
	self changed: #bindKeyCombination:toAction: with: {aShortcut . aBlock}