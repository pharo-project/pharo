typing/selecting keys
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	self hasSelection 
		ifTrue: [self addDeleteSelectionUndoRecord].
	self addString: aKeyboardEvent keyCharacter asString.
	^false