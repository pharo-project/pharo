*SmartSuggestions
privateAccessorsInstVarNamed: aVariableName from:  aClass.
	^ RBCreateAccessorsForVariableRefactoring 
			model: environment
			variable: aVariableName
			class: aClass
			classVariable: false