preconditions
preconditions
	^(selectors inject: RBCondition empty
		into: [:cond :each | cond & (RBCondition definesSelector: each in: class)]) 
			& (RBCondition withBlock: 
						[self checkSuperMethods.
						true])