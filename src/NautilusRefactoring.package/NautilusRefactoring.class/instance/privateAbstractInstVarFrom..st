private-inst var
privateAbstractInstVarFrom: aClass
	^ self class: aClass andInstVariable: [ :class :variable | 
		RBAbstractInstanceVariableRefactoring
			model: environment
			variable: variable
			class: class ]