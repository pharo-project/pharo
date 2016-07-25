preconditions
preconditionIsNotMetaclass: aClass
	^ ((RBCondition isMetaclass: aClass) errorMacro: 'Cannot remove just the metaclass') not