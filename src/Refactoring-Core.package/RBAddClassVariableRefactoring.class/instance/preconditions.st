preconditions
preconditions
	^(RBCondition isMetaclass: class) not 
		& (RBCondition isValidClassVarName: variableName for: class) 
			& (RBCondition hierarchyOf: class definesVariable: variableName asString) 
					not 
			& (RBCondition isGlobal: variableName in: self model) not