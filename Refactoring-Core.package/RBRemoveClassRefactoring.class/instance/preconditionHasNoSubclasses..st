preconditions
preconditionHasNoSubclasses: aClass
	^ (RBCondition withBlock: [ aClass subclasses isEmpty ])
		errorMacro: self class cannotRemoveTopLevelClassErrorMesssage;
		yourself