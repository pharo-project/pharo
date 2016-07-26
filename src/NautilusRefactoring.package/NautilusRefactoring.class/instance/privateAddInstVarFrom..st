private-inst var
privateAddInstVarFrom: aClass
	^ RBAddInstanceVariableRefactoring 
		model: environment 
		variable: (self request: 'Enter the new variable name:' initialAnswer: 'inst') 
		class: aClass theNonMetaClass