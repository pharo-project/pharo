private-inst var
privatePushDownInstVarFrom: aClass
	^ self class: aClass andInstVariable: [ :class :variable | 
		RBPushDownInstanceVariableRefactoring
			model: environment
			variable: variable
			class: class ]