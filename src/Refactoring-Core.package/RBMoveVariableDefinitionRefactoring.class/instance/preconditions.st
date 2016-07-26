preconditions
preconditions
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[| methodSource |
					interval first <= interval last 
						ifFalse: [self refactoringFailure: 'You must select a variable name'].
					methodSource := class sourceCodeFor: selector.
					methodSource size >= interval last 
						ifFalse: [self refactoringFailure: 'Invalid range for variable'].
					name := methodSource copyFrom: interval first to: interval last.
					(self checkInstanceVariableName: name in: class) 
						ifFalse: 
							[self 
								refactoringFailure: name , ' does not seem to be a valid variable name.'].
					parseTree := class parseTreeFor: selector.
					self checkParseTree.
					true])