preconditions
preconditions
	^(RBCondition isValidClassName: newClassName) 
		& (RBCondition isGlobal: newClassName in: self model) not 
			& (RBCondition isValidInstanceVariableName: referenceVariableName for: class) 
			& (RBCondition hierarchyOf: class definesVariable: referenceVariableName) 
					not 
			& (RBCondition isGlobal: referenceVariableName in: self model) not 
			& (RBCondition definesTemporaryVariable: referenceVariableName in: class) 
					not