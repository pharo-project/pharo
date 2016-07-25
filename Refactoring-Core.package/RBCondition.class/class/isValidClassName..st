instance creation
isValidClassName: aString
	^self new
		type: (Array with: #validClassName with: aString)
		block: [self validClassName: aString]
		errorString: aString , ' is <1?:not >a valid class name'