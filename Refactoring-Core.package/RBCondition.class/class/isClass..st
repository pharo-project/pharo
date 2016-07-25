instance creation
isClass: anObject
	^self new
		type: (Array with: #IsClass with: anObject)
		block: [anObject isBehavior]
		errorString: anObject printString , ' is <1?:not >a behavior'