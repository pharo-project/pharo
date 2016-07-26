testing
hierarchyDefinesClassVariable: aString 
	(self definesClassVariable: aString) ifTrue: [^true].
	^(self allSubclasses 
		detect: [:each | each directlyDefinesClassVariable: aString]
		ifNone: [nil]) notNil