protocol-shortcuts
bindMenuKeyCombination: aShortcut toAction: aBlock

	self widget ifNotNil: [ :w | w bindKeyCombination: aShortcut toAction: aBlock ]