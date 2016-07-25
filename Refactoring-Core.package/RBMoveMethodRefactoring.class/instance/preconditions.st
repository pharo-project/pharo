preconditions
preconditions
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[self buildParseTree.
					self checkForPrimitiveMethod.
					self checkForSuperReferences.
					self checkAssignmentsToVariable.
					self getClassesToMoveTo.
					self getArgumentNameForSelf.
					self checkTemporaryVariableNames.
					self getNewMethodName.
					true])