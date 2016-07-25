protocol-shortcuts
removeMenuKeyCombination: aShortcut

	self widget ifNotNil: [ :w | w removeKeyCombination: aShortcut ]