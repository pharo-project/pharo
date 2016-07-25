preconditions
preconditions
	^((RBCondition withBlock: [class notNil and: [class isMeta not]]) 
			& (RBCondition isValidClassName: newName) 
			& (RBCondition isGlobal: newName in: self model) not) |
			(RBCondition withBlock: [ self refactoringFailure: newName , ' is not a valid class name'])