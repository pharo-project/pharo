instance creation
isMetaclass: anObject 
	^self new type: (Array with: #IsMetaclass with: anObject)
		block: [anObject isMeta]
		errorString: anObject printString , ' is <1?:not >a metaclass'