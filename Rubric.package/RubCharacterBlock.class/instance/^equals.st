comparing
= aCharacterBlock

	self species = aCharacterBlock species
		ifTrue: [^stringIndex = aCharacterBlock stringIndex]
		ifFalse: [^false]