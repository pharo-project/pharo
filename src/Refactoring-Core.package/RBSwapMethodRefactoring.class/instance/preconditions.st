preconditions
preconditions
	^ (RBCondition definesSelector: selector in: class)
		& (RBCondition definesSelector: selector in: target) not
		& (RBCondition withBlock: [ self checkInstVars. true ])