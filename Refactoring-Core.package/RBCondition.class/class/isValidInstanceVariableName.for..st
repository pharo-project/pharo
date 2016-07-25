instance creation
isValidInstanceVariableName: aString for: aClass 
	^self new 
		type: (Array 
				with: #validInstVarName
				with: aString
				with: aClass)
		block: [self checkInstanceVariableName: aString in: aClass]
		errorString: aString , ' is <1?:not >a valid instance variable name'