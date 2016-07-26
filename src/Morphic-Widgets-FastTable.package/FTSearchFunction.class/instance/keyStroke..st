event handling
keyStroke: anEvent
	(anEvent anyModifierKeyPressed
		or: [ anEvent keyCharacter isAlphaNumeric not ])
		ifTrue: [ ^ false ].
	currentSelIndex := self table selectedRowIndex.
	currentHighlightedIndexes := self table highlightedRowIndexes.
	self showSearchFieldFromKeystrokeEvent: anEvent.
	^ true