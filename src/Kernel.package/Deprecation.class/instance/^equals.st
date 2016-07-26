comparing
= anObject
	^self class == anObject class
	  and: [methodReference = anObject methodReference
	  and: [methodReference
			ifNil: [explanationString = anObject explanationString]
			ifNotNil: [true]]]