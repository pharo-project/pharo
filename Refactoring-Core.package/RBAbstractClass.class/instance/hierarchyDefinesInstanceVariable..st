testing
hierarchyDefinesInstanceVariable: aString 
	(self definesInstanceVariable: aString) ifTrue: [^true].
	^(self allSubclasses 
		detect: [:each | each directlyDefinesInstanceVariable: aString]
		ifNone: [nil]) notNil