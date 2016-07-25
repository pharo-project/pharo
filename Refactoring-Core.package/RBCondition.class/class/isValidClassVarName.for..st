instance creation
isValidClassVarName: aString for: aClass
	^self new
		type: (Array with: #validClassVarName with: aString with: aClass)
		block: [self checkClassVarName: aString in: aClass]
		errorString: aString , ' is <1?:not >a valid class variable name'