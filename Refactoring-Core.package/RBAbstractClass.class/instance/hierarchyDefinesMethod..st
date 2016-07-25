testing
hierarchyDefinesMethod: aSelector 
	(self definesMethod: aSelector) ifTrue: [^true].
	^self subclassRedefines: aSelector