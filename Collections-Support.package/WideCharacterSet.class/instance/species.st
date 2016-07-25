comparing
species
	^self hasWideCharacters
		ifTrue: [WideCharacterSet]
		ifFalse: [CharacterSet]