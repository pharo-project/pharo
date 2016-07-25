instance creation
isValidMethodName: aString for: aClass
	^self new
		type: (Array with: #validMethodName with: aString with: aClass)
		block: [self checkMethodName: aString in: aClass]
		errorString: aString printString , ' is <1?:not >a valid method name'