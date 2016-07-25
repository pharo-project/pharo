accessing
typesFor: anInstVarName 
	^variableTypes at: anInstVarName ifAbsent: [Set new]