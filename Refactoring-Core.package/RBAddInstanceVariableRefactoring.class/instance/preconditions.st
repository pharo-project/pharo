preconditions
preconditions
	^(RBCondition isValidInstanceVariableName: variableName for: class) 
		& (RBCondition hierarchyOf: class definesVariable: variableName) not 
			& (RBCondition isGlobal: variableName in: self model) not