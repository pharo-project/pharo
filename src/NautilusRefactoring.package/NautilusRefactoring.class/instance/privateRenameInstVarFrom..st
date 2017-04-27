private-inst var
privateRenameInstVarFrom: aClass

	^ self class: aClass andInstVariable: [ :class :variable | 
		RBRenameInstanceVariableRefactoring
			model: environment
			rename: variable
			to: (self request: self newVariableRequestText initialAnswer: variable) asSymbol
			in: class 
	]