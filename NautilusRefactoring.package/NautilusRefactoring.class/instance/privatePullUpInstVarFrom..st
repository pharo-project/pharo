private-inst var
privatePullUpInstVarFrom: aClass
	^ self class: aClass andInstVariable: [ :class :variable | 
		RBPullUpInstanceVariableRefactoring 
			model: environment
			variable: variable
			class: class superclass ]