*UnifiedFFI
offsetReadStructFieldAt: offsetVariableName
	^ '^ {1} fromHandle: (handle structAt: {2} length: {1} byteSize)' 
		format: { 
			referentClass name.
			offsetVariableName }
