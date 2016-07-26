private building
tagsForVariable: aString from: aContext
	^ (aContext reservedNames includes: aString) 
		ifTrue: [ 'implicit' ]
		ifFalse: [ self variableTypeOf: aString in: aContext ]
			
	