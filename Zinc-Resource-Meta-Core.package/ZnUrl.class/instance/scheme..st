accessing
scheme: anObject
	anObject isNil
		ifTrue: [ scheme := nil ]
		ifFalse: [ scheme := anObject asLowercase asSymbol ]