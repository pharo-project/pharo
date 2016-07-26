private-class var
privateRemoveClassVarFrom: aClass
	^ self class: aClass andClassVariable: [ :class :variable | 
		RBRemoveClassVariableRefactoring
			model: environment
			variable: variable
			class: class ]