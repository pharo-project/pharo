Extension { #name : #RBVariableNode }

{ #category : #'*NautilusRefactoring' }
RBVariableNode >> executeRenameRefactoringBy: aNautilusRefactoring [
	self executeRenameRefactoringBy: aNautilusRefactoring for: self

]

{ #category : #'*NautilusRefactoring' }
RBVariableNode >> executeRenameRefactoringBy: aNautilusRefactoring for: aNode [
	variable executeRenameRefactoringBy: aNautilusRefactoring

]
