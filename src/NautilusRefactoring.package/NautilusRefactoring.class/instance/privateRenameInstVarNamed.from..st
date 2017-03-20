private-inst var
privateRenameInstVarNamed: variable from: aClass

	^ RBRenameInstanceVariableRefactoring
		model: environment
		rename: variable
		to: (self request: self newVariableRequestText initialAnswer: variable) asSymbol
		in: aClass