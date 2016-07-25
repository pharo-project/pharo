testing
hierarchyDefinesVariable: aString 
	(self definesVariable: aString) ifTrue: [^true].
	^(self allSubclasses 
		detect: [:each | each directlyDefinesVariable: aString]
		ifNone: [nil]) notNil