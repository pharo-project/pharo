private-inst var
privateRemoveInstVarFrom: aClass
	^ self class: aClass andInstVariable: [ :class :variable | 
		RBRemoveInstanceVariableRefactoring
			model: environment
			variable: variable
			class: class ]