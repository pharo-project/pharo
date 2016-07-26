primitive error codes
removeAndRenameLastTempIfErrorCode
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 temporaries last
			name: primitiveErrorVariableName
			key: primitiveErrorVariableName
			code: temporaries last code.
		 temporaries removeLast].