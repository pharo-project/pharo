preconditions
preconditions
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition hierarchyOf: class
				definesVariable: temporaryVariableName asString) not 
			& (RBCondition withBlock: 
						[self checkForValidTemporaryVariable.
						true])