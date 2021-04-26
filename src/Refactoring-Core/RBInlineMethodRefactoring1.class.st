Class {
	#name : #RBInlineMethodRefactoring1,
	#superclass : #RBInlineMethodRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBInlineMethodRefactoring1 >> preconditions [
	^(RBCondition definesSelector: sourceSelector in: class) 
		& (RBCondition withBlock: 
					[self findSelectedMessage.
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
]
