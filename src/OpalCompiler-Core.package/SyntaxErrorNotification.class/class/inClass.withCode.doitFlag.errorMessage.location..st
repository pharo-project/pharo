exceptionInstantiator
inClass: aClass withCode: codeString doitFlag: doitFlag errorMessage: errorString location: location
	| sourceCode |
	sourceCode := (self new
		setClass: aClass
		code: codeString
		doitFlag: doitFlag
		errorMessage: errorString
		location: location) signal.
	sourceCode ifNil: [ (OCAbortCompilation sourceCode: sourceCode) signal ].
	^ sourceCode