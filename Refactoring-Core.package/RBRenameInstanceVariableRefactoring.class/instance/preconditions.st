preconditions
preconditions
	^(RBCondition isValidInstanceVariableName: newName for: class) 
		& (RBCondition definesInstanceVariable: variableName in: class) 
			& (RBCondition hierarchyOf: class definesVariable: newName) not 
			& (RBCondition isGlobal: newName in: self model) not