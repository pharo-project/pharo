comparing
= anObject
	^self class == anObject class
	  and: [context = anObject context
	  and: [context
			ifNil: [explanationString = anObject explanationString]
			ifNotNil: [true]]]