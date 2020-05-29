Extension { #name : #RBSelfKind }

{ #category : #'*NautilusRefactoring' }
RBSelfKind >> executeRenameRefactoringBy: aNautilusRefactoring [

	self inform: variable class name, ' not supports rename'
]
