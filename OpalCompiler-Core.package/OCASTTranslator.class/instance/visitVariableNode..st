visitor-double dispatching
visitVariableNode: aVariableNode
	aVariableNode binding emitValue: methodBuilder
	
	