preconditions
preconditions
	| condition |
	condition := selectors 
		inject: RBCondition empty
		into: [ :cond :each | 
			cond & (RBCondition definesSelector: each in: class) 
				& (RBCondition subclassesOf: class referToSelector: each) not].
	^condition & (RBCondition isAbstractClass: class)