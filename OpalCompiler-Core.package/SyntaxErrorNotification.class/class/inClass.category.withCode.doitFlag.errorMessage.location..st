exceptionInstantiator
inClass: aClass category: aCategory withCode: codeString doitFlag: doitFlag errorMessage: errorString location: location
	^ (self new
		setClass: aClass
		category: aCategory 
		code: codeString
		doitFlag: doitFlag
		errorMessage: errorString
		location: location) signal