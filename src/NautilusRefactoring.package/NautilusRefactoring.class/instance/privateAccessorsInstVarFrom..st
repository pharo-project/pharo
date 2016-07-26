private-inst var
privateAccessorsInstVarFrom: aClass
	^ self class: aClass andInstVariable: [ :class :variable | 
		RBCreateAccessorsForVariableRefactoring 
			model: environment
			variable: variable
			class: class
			classVariable: false ]