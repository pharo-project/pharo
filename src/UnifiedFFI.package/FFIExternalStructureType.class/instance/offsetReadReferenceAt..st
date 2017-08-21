emitting code
offsetReadReferenceAt: offsetVariableName
	^ '^ {1} fromHandle: (handle referenceStructAt: {2} length: {1} byteSize)'
		format: { self objectClass name. offsetVariableName }