private
offsetVariableNameFor: fieldName
	^ (self offsetFieldPrefix, '_', fieldName asUppercase) asSymbol