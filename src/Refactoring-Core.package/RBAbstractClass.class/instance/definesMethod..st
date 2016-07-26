testing
definesMethod: aSelector 
	(self directlyDefinesMethod: aSelector) ifTrue: [^true].
	^self superclass notNil and: [self superclass definesMethod: aSelector]