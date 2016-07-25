preconditions
preconditions
	| cond |
	cond := ((RBCondition isMetaclass: superclass) 
				errorMacro: 'Superclass must not be a metaclass') not.
	cond := subclasses inject: cond
				into: 
					[:sub :each | 
					sub 
						& ((RBCondition isMetaclass: each) 
								errorMacro: 'Subclass must <1?not :>be a metaclass') not 
							& (RBCondition isImmediateSubclass: each of: superclass)].
	^cond & (RBCondition isValidClassName: className) 
		& (RBCondition isGlobal: className in: self model) not 
			& (RBCondition isSymbol: category) 
			& ((RBCondition withBlock: [category isEmpty not]) 
					errorMacro: 'Invalid category name')