printing
printOn: aStream

	aStream nextPutAll: 'a CharacterBlock with index '.
	stringIndex printOn: aStream.
	(text ~~ nil and: [text size> 0 and: [stringIndex between: 1 and: text size]])
		ifTrue: [aStream nextPutAll: ' and character '.
				(text at: stringIndex) printOn: aStream].
	aStream nextPutAll: ' and rectangle '.
	super printOn: aStream.
	textLine ifNotNil: [aStream cr; nextPutAll: ' in '.
				textLine printOn: aStream].
