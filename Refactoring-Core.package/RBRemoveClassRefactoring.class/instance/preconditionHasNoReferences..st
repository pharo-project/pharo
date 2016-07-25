preconditions
preconditionHasNoReferences: aClass

	| env |
	
	env := self environmentWithReferencesTo: aClass.
	^ (RBCondition withBlock: [ env isEmpty ])
		errorMacro: aClass , ' is referenced.<n>Browse references?';
		errorBlock: [ self openBrowserOn: (env) ];
		yourself