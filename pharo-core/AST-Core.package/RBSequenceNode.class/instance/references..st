testing
references: aVariableName 
	^statements anySatisfy: [:each | each references: aVariableName]