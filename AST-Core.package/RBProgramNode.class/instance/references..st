testing
references: aVariableName 
	^self children anySatisfy: [:each | each references: aVariableName]