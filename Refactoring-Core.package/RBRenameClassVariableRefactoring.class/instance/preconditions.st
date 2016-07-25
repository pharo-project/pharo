preconditions
preconditions
	^(RBCondition isMetaclass: class) not 
		& (RBCondition isValidClassVarName: newName asString for: class) 
			& (RBCondition definesClassVariable: variableName asString in: class) 
			& (RBCondition hierarchyOf: class definesVariable: newName asString) not 
			& (RBCondition isGlobal: newName asString in: self model) not