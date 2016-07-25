private-class var
privatePushDownClassVarFrom: aClass
	^ self class: aClass andClassVariable: [ :class :variable | 
		RBPushDownClassVariableRefactoring
			model: environment
			variable: variable
			class: class ]