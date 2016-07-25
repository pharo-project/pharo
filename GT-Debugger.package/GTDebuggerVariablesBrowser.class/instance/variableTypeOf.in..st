building
variableTypeOf: aString in: aContext
	^ (aContext receiver class classThatDefinesInstanceVariable: aString) notNil
			ifTrue: [ 'attribute' ]
			ifFalse: [ (aContext method argumentNames includes: aString)
				ifTrue: [ 'parameter' ]
				ifFalse: [ 'temp' ] ]
			
	