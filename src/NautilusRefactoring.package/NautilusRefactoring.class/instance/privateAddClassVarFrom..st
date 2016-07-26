private-class var
privateAddClassVarFrom: aClass
	^ RBAddClassVariableRefactoring 
		model: environment 
		variable: (self request: 'Enter the new variable name:' initialAnswer: 'Var')
		class: aClass theNonMetaClass