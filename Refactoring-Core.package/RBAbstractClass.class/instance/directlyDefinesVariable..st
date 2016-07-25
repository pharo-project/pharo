testing
directlyDefinesVariable: aVariableName 
	^(self directlyDefinesClassVariable: aVariableName) 
		or: [self directlyDefinesInstanceVariable: aVariableName]