private-class var
privateAbstractClassVarFrom: aClass
	^ self class: aClass andClassVariable: [ :class :variable | 
		RBAbstractClassVariableRefactoring
			model: environment
			variable: variable
			class: class ]