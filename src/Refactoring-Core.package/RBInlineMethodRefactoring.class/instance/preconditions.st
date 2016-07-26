preconditions
preconditions
	^(RBCondition definesSelector: sourceSelector in: class) 
		& (RBCondition withBlock: 
					[self findSelectedMessage.
					self isOverridden 
						ifTrue: 
							[self 
								refactoringWarning: ('<1p>>><2s> is overriden. Do you want to inline it anyway?' 
										expandMacrosWith: self inlineClass
										with: self inlineSelector)].
					self parseInlineMethod.
					self isPrimitive 
						ifTrue: [self refactoringError: 'Cannot inline primitives'].
					self checkSuperMessages.
					self rewriteInlinedTree.
					(sourceMessage parent isReturn or: [self hasMultipleReturns not]) 
						ifFalse: 
							[self 
								refactoringError: 'Cannot inline method since it contains multiple returns that cannot be rewritten'].
					true])