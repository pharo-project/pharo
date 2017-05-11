private-class var
privateAddClassVarFrom: aClass
	^ RBAddClassVariableRefactoring 
		model: environment 
		variable: (self request: self newVariableRequestText initialAnswer: 'Var')
		class: aClass theNonMetaClass