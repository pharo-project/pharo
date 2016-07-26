basic-completion
selectorChooserUpdate
	selectorChooser ifNil: [^ self].
	self selectorChooserClose.
	self selectorChooseQuerySymbol 