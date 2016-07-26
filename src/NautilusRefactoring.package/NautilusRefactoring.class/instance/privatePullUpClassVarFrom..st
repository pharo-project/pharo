private-class var
privatePullUpClassVarFrom: aClass
	^ self class: aClass andClassVariable: [ :class :variable | 
		RBPullUpClassVariableRefactoring 
			model: environment
			variable: variable
			class: class ]