exceptionInstantiator
inClass: aClass withCode: codeString doitFlag: doitFlag errorMessage: errorString location: location
	^ (self new
		setClass: aClass
		code: codeString
		doitFlag: doitFlag
		errorMessage: errorString
		location: location) signal