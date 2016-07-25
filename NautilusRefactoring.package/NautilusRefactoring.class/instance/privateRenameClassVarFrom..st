private-class var
privateRenameClassVarFrom: aClass
	^ self
		class: aClass
		andClassVariable: [ :class :variable | 
			RBRenameClassVariableRefactoring
				model: environment
				rename: variable
				to: (self request: 'Enter the new variable name :' initialAnswer: variable) asSymbol
				in: class ]