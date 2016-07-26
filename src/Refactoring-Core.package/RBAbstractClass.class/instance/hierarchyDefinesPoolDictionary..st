testing
hierarchyDefinesPoolDictionary: aString 
	(self definesPoolDictionary: aString) ifTrue: [^true].
	^(self allSubclasses 
		detect: [:each | each directlyDefinesPoolDictionary: aString]
		ifNone: [nil]) notNil