selector-chooser
updateSelectorChooserFrom: aTextMorph
	selectorChooser ifNil: [^ self].
	self closeSelectorChooser.
	self querySymbolFrom: aTextMorph