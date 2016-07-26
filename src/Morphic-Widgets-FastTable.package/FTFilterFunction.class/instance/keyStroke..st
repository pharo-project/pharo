event handling
keyStroke: anEvent
	(anEvent keyCharacter = Character escape and: [ initialDataSource isNotNil ])
		ifTrue: [ ^ self reinitializeTable ].	"If the user escape after a search, he want the full data source again so we reinitialize the table."
	(anEvent anyModifierKeyPressed or: [ anEvent keyCharacter isAlphaNumeric not ])
		ifTrue: [ ^ false ].
	self showFilterFieldFromKeystrokeEvent: anEvent.
	^ true