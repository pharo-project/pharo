accessing
nextPut: aCharacter
	aCharacter isInteger 
		ifTrue: [ ^ super nextPut: aCharacter ].
	(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
		 ifTrue: [
			aCharacter = Cr
				ifTrue: [ converter nextPutAll: (LineEndStrings at: lineEndConvention) toStream: self ]
				ifFalse: [ converter nextPut: aCharacter toStream: self ].
			^aCharacter ].
	^ self converter nextPut: aCharacter toStream: self
