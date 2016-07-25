preconditions
preconditions
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[self verifySelectedInterval.
					true])