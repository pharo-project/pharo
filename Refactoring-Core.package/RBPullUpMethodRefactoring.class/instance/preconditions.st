preconditions
preconditions
	^(selectors inject: (RBCondition hasSuperclass: class)
		into: [:cond :each | cond & (RBCondition definesSelector: each in: class)]) 
			& (RBCondition withBlock: 
						[self checkInstVars.
						self checkClassVars.
						self checkSuperclass.
						self checkSuperMessages.
						true])