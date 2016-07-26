private-inst var
privateRenameInstVarNamed: variable from: aClass
	^ RBRenameInstanceVariableRefactoring
		model: environment
		rename: variable
		to: (self request: 'Enter the new variable name :' initialAnswer: variable) asSymbol
		in: aClass