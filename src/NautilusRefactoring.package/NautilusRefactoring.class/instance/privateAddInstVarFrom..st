private-inst var
privateAddInstVarFrom: aClass
	^ RBAddInstanceVariableRefactoring 
		model: environment 
		variable: (self request: self newVariableRequestText initialAnswer: 'inst') 
		class: aClass theNonMetaClass