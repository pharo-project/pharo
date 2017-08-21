accessing
nextPut: aCharacter
	^ aCharacter isInteger 
		ifTrue: [ super nextPut: aCharacter asCharacter ]
		ifFalse: [ self converter nextPut: aCharacter toStream: self ]