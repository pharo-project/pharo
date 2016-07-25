preconditions
myConditions
	^RBCondition withBlock: 
			[oldSelector numArgs + 1 = newSelector numArgs 
				ifFalse: 
					[self refactoringFailure: newSelector printString 
								, ' doesn''t have the proper number of arguments.'].
			self verifyInitializationExpression.
			true]