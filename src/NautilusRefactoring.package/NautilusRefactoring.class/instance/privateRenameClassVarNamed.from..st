private-class var
privateRenameClassVarNamed: variable from: aClass
	^ RBRenameClassVariableRefactoring
		model: environment
		rename: variable
		to: (self request: 'Enter the new variable name :' initialAnswer: variable) asSymbol
		in: aClass