methods
referencesConditionFor: aClass 
	| environment association |
	^(RBCondition withBlock: 
			[association := Smalltalk associationAt: aClass name
						ifAbsent: [self refactoringError: 'Could not find class'].
			environment := (self environment referencesTo: association) 
						| (self environment referencesTo: aClass name).
			environment isEmpty])
		errorMacro: aClass , ' is referenced.<n>Browse references?';
		errorBlock: [environment openEditor];
		yourself