model protocol
on: aModel text: aGetTextSelector accept: aSetTextSelector readSelection: aReadSelectionSelector menu: aGetMenuSelector
	self getTextSelector: aGetTextSelector.
	self setTextSelector: aSetTextSelector.
	self getSelectionSelector: aReadSelectionSelector.
	aGetMenuSelector
		ifNil: [ self textArea forbidMenu ]
		ifNotNil: [ self menuProvider: aModel selector: aGetMenuSelector ].
	self on: aModel