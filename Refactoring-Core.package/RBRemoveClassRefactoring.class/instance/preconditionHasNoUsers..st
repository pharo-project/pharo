preconditions
preconditionHasNoUsers: aClassOrTrait
	^ (RBCondition withBlock: [ aClassOrTrait realClass users isEmpty ])
		errorMacro: aClassOrTrait name , ' Trait is used.<n>Browse classes using it?';
		errorBlock: [ self openBrowserOn: (self environmentWithUsersOf: aClassOrTrait) ];
		yourself