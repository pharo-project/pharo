testing
definesVariable: aVariableName 
	^(self definesClassVariable: aVariableName) 
		or: [self definesInstanceVariable: aVariableName]