preconditions
preconditions
	^subclasses 
		inject: ((RBCondition isMetaclass: parent) 
				errorMacro: 'Superclass must not be a metaclass') not 
				& (RBCondition isValidClassName: className) 
					& (RBCondition isGlobal: className in: self model) not
		into: 
			[:sub :each | 
			sub 
				& ((RBCondition isMetaclass: each) 
						errorMacro: 'Subclass must <1?not :>be a metaclass') not 
					& (RBCondition isImmediateSubclass: each of: parent)]