*UnifiedFFI
offsetWriteStructFieldAt: offsetVariableName with: valueName
	^ 'handle structAt: {1} put: {2} getHandle length: {3} byteSize' 
		format: {
			offsetVariableName. 
			valueName. 
			referentClass name }