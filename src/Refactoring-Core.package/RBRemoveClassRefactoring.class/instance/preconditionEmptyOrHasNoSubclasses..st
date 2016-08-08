preconditions
preconditionEmptyOrHasNoSubclasses: aClass
	^ (RBCondition hasSubclasses: aClass) not
		|
			((RBCondition isEmptyClass: aClass)
				&
					((RBCondition withBlock: [ aClass superclass notNil ])
						errorMacro: 'Cannot remove top level class<n>when it has subclasses';
						yourself))