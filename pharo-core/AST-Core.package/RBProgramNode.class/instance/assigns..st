testing
assigns: aVariableName 
	^self children anySatisfy: [:each | each assigns: aVariableName]