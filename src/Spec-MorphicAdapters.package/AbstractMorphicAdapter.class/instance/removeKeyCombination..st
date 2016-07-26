protocol-shortcuts
removeKeyCombination: aShortcut

	self widget ifNotNil: [ :w | w removeKeyCombination: aShortcut ]