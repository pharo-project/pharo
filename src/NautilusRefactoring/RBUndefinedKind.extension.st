Extension { #name : #RBUndefinedKind }

{ #category : #'*NautilusRefactoring' }
RBUndefinedKind >> executeRenameRefactoringBy: aNautilusRefactoring [

	self inform: variable class name, ' not supports rename'
]
