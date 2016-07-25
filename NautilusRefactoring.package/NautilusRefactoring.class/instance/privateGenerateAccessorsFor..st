private-class
privateGenerateAccessorsFor: class
	^ class instVarNames
		collect: [ :aVarName | 
			RBCreateAccessorsForVariableRefactoring
				model: environment
				variable: aVarName
				class: class
				classVariable: false ]