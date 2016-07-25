printing
printOn: aStream
	aStream nextPutAll: 'a CharacterBlock with index '.
	stringIndex printOn: aStream.
	aStream nextPutAll: ' and surface '.
	self surface printOn: aStream.
	textLine
		ifNotNil: [ 
			aStream
				cr;
				nextPutAll: ' in '.
			textLine printOn: aStream ]