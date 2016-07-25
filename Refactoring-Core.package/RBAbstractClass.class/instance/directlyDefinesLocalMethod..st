testing
directlyDefinesLocalMethod: aSelector 
	self isDefined 
		ifTrue: 
			[(self hasRemoved: aSelector) ifTrue: [^false].
			(self realClass includesLocalSelector: aSelector) ifTrue: [^true]].
	^newMethods notNil and: [newMethods includesKey: aSelector]