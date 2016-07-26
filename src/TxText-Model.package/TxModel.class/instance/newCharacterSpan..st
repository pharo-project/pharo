private-factory methods
newCharacterSpan: aString
	^ self newCharacterSpan
		characters: aString;
		yourself