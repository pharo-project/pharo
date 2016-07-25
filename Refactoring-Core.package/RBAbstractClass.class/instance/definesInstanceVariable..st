testing
definesInstanceVariable: aString 
	(self directlyDefinesInstanceVariable: aString) ifTrue: [^true].
	^self superclass notNil 
		and: [self superclass definesInstanceVariable: aString]