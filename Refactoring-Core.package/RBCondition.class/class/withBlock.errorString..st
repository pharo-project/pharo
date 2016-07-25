instance creation
withBlock: aBlock errorString: aString 
	^self new type: #unknown
		block: aBlock
		errorString: aString