private-class var
privateAccessorsClassVarFrom: aClass
	^ self class: aClass andClassVariable: [ :class :variable | 
		RBCreateAccessorsForVariableRefactoring 
			model: environment
			variable: variable
			class: class
			classVariable: true ]