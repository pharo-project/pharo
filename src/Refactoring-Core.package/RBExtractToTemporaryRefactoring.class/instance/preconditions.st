preconditions
preconditions
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition isValidInstanceVariableName: newVariableName for: class) 
			& (RBCondition withBlock: 
						[self verifySelectedInterval.
						self checkVariableName.
						true])