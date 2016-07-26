instance creation
isEmptyClass: anObject 
	^self new type: (Array with: #IsEmptyClass with: anObject)
		block: 
			[anObject classVariableNames isEmpty
				and: [anObject instanceVariableNames isEmpty and: [anObject selectors isEmpty]]]
		errorString: anObject printString , ' is <1?:not > empty'